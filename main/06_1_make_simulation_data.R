rm(list = ls())
library(magrittr)

# load data ----
# set constant ----
# monte carlo simulation to get nice sample size identifying match effect
set.seed(1)
num_simulation <-
  100
list_num_time <-
  c(
    # 10,
    # 20,
    # 30,
    # 40,
    50,
    100,
    200
  )
list_simulation <-
  c(1:num_simulation)
## set stationarity parameter ----
arima_list <-
  list(
    "AR1_I0" = c(1,0,0),
    #"AR1_I1" = c(1,1,0),
    "AR0_I1" = c(0,1,0)
    )
ar1_parameter_unemployed <-
  0.2
ar1_parameter_vacancy <-
  0.2
ar1_parameter_efficiency <-
  0.2
arima_setting_unemployed <-
  c(1,0,0)
arima_setting_vacancy <-
  c(1,0,0)
arima_setting_efficiency <-
  c(1,0,0)
## set matching function specification ----
specification_list <-
  c(
    "cobb_douglas",
    "perfect_substitute",
    "fixed_proportion"
  )
CRS_gamma_parameter <-
  0.3#0.001
## set independence parameter ----
dependency_V_of_A_list <-
  c(
    0,
    0.1,
    0.2
  )


# generate data ----
generate_uvah_data <-
  function(
    time,
    arima_setting_unemployed,
    arima_setting_vacancy,
    arima_setting_efficiency,
    ar1_parameter_unemployed,
    ar1_parameter_vacancy,
    ar1_parameter_efficiency,
    dependency_V_of_A,
    matching_function_specification,
    CRS_gamma_parameter
  ){
    if(arima_setting_unemployed[2] == 0){
      # stationary
      unemployed <- 
        arima.sim(
          list(
            order = arima_setting_unemployed, # ar 1
            ar = ar1_parameter_unemployed
          ), 
          n = length(time)
        )
    }else{
      # nonstationary
      unemployed <- 
        arima.sim(
          list(
            order = arima_setting_unemployed
          ), 
          n = length(time)
        )
    }
    unemployed <-
      (unemployed + abs(min(unemployed)) + 10) * 100 # avoid negative
    if(arima_setting_vacancy[2] == 0){
      # stationary
      vacancy <- 
        arima.sim(
          list(
            order = arima_setting_vacancy, # ar 1
            ar = ar1_parameter_vacancy
          ), 
          n = length(time)
        )
    }else{
      # nonstationary
      vacancy <- 
        arima.sim(
          list(
            order = arima_setting_vacancy
          ), 
          n = length(time)
        )
    }
    vacancy <-
      (vacancy + abs(min(vacancy)) + 10) * 100 # avoid negative
    if(arima_setting_efficiency[2] == 0){
      # stationary
      efficiency <- 
        arima.sim(
          list(
            order = arima_setting_efficiency, # ar 1
            ar = ar1_parameter_efficiency
          ), 
          n = length(time)
        )
    }else{
      # nonstationary
      efficiency <- 
        arima.sim(
          list(
            order = arima_setting_efficiency
          ), 
          n = length(time)
        )
    }
    # include dependency_V_of_A in V
    if(dependency_V_of_A > 0){
      std_vacancy <- 
        (vacancy - mean(vacancy))/sd(vacancy)
      std_efficiency <- 
        (efficiency - mean(efficiency))/sd(efficiency)
      cor_mat <- 
        matrix(c(1, dependency_V_of_A, dependency_V_of_A, 1), nrow = 2, byrow = T)
      chol_mat <- 
        chol(cor_mat)
      old_random <- 
        cbind(std_vacancy[1:length(unemployed)], std_efficiency[1:length(unemployed)])
      new_random <- 
        old_random %*% chol_mat
      cor(old_random[1:length(unemployed),])
      cor(new_random[1:length(unemployed),])
      recovered_efficiency <-
        new_random[,2]* sd(efficiency[1:length(unemployed)]) + 
        mean(efficiency[1:length(unemployed)])
      efficiency <-
        recovered_efficiency
    }
    efficiency <-
      efficiency + abs(min(efficiency)) + 10 # avoid negative
    normalized_efficiency <-
      (
        efficiency/
          efficiency[1]
          # quantile(
          #   efficiency, 
          #   probs = 0.50 
          #   ) 
      )* 100 #/10 # initial value = 1/10
    #cat(cor(normalized_efficiency,vacancy))
    
    # format data length
    time <-
      1:length(unemployed)
    efficiency <-
      efficiency[1:length(unemployed)]
    normalized_efficiency <-
      normalized_efficiency[1:length(unemployed)]
    data <-
      cbind(
        time,
        unemployed,
        vacancy,
        efficiency,
        normalized_efficiency
      ) %>% 
      tibble::as_tibble() 
    if(matching_function_specification == "cobb_douglas"){
      data <-
        data %>% 
        dplyr::mutate(
          hire =
            ((normalized_efficiency *unemployed)^CRS_gamma_parameter*
            vacancy^(1 - CRS_gamma_parameter))/10
          )
    }else if(matching_function_specification == "perfect_substitute"){
      data <-
        data %>% 
        dplyr::mutate(
          hire =
            (CRS_gamma_parameter * 
            normalized_efficiency *
            #efficiency *
            unemployed +
            CRS_gamma_parameter *
            vacancy)/10
        )
    }else if(matching_function_specification == "fixed_proportion"){
      data <-
        data %>% 
        dplyr::mutate(
          hire =
            (min(CRS_gamma_parameter * 
                  normalized_efficiency *
                  #efficiency *
                  unemployed,
                CRS_gamma_parameter *
                  vacancy))/10
        )
    }
    data <-
      data %>% 
      dplyr::mutate(
        overhire_check =
          (unemployed <= hire) |
          (vacancy <= hire),
        specification =
          matching_function_specification,
        CRS_gamma_parameter =
          CRS_gamma_parameter,
        industry_group_name = 1
      ) 
    return(data)
  }

generate_data <-
  function(
    time,
    list_simulation,
    arima_setting_unemployed,
    arima_setting_vacancy,
    arima_setting_efficiency,
    ar1_parameter_unemployed,
    ar1_parameter_vacancy,
    ar1_parameter_efficiency,
    dependency_V_of_A,
    matching_function_specification,
    CRS_gamma_parameter
  ){
    data <-
      expand.grid(
        time,
        list_simulation
      ) %>% 
      tibble::as_tibble() 
    colnames(data) <-
      c(
        "simulation_id",
        "time"
      )
    for(ss in 1:length(list_simulation)){
      simulation_id <-
        list_simulation[ss]
      temp_data <-
        generate_uvah_data(
          time,
          arima_setting_unemployed,
          arima_setting_vacancy,
          arima_setting_efficiency,
          ar1_parameter_unemployed,
          ar1_parameter_vacancy,
          ar1_parameter_efficiency,
          dependency_V_of_A,
          matching_function_specification,
          CRS_gamma_parameter
        )
      if(simulation_id == 1){
        temp_data <-
          cbind(
            simulation_id,
            temp_data
          )
        # initial creation
        data <-
          temp_data
      }else{
        temp_data <-
          cbind(
            simulation_id,
            temp_data
          )
        # merge subsequent data
        data <-
          rbind(
            data,
            temp_data
          )
      }
    }
    return(data)
  }



## benchmark data ----
for(nn in 1:length(list_num_time)){
  for(mm in 1:length(specification_list)){
    for(rr in 1:length(arima_list)){
      for(qq in 1:length(dependency_V_of_A_list)){
        dependency_V_of_A <-
          dependency_V_of_A_list[qq]
        target_arima <-
          arima_list[[rr]]
        fixed_ar <-
          arima_list[[1]]
        target_arima_name <-
          names(arima_list)[rr]
        CRS_gamma_parameter <-
          0.3
        temp_nn <-
          list_num_time[nn]
        matching_function_specification <-
          specification_list[mm]
        time <-
          c(1:temp_nn)
        data <-
          generate_data(
            time,
            list_simulation,
            # nonstationarity only on A
            # arima_setting_unemployed = target_arima,
            # arima_setting_vacancy = target_arima,
            arima_setting_unemployed = fixed_ar,
            arima_setting_vacancy = fixed_ar,
            arima_setting_efficiency = target_arima,
            ar1_parameter_unemployed,
            ar1_parameter_vacancy,
            ar1_parameter_efficiency,
            dependency_V_of_A,
            matching_function_specification,
            CRS_gamma_parameter
          )
        filename <-
          paste(
            "output/monte_carlo_data_",
            "num_time_",
            temp_nn,
            "_",
            matching_function_specification,
            "_",
            CRS_gamma_parameter,
            "_",
            target_arima_name,
            "_",
            "va_dependency_",
            dependency_V_of_A,
            ".rds",
            sep = ""
          )
        cat(filename,"\n")
        saveRDS(
          data,
          file = filename)
      }
      
    }
    
  }
}
