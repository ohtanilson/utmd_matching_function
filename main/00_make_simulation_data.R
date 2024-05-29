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
    10,
    20,
    30,
    40,
    50,
    100,
    500
  )
list_simulation <-
  c(1:num_simulation)
ar1_parameter_unemployed <-
  0.2
ar1_parameter_vacancy <-
  0.2
ar1_parameter_efficiency <-
  0.2
hire_parameter <-
  0.001
scale_parameter <-
  1e4


# generate data ----
generate_ar1_data <-
  function(
    time,
    ar1_parameter_unemployed,
    ar1_parameter_vacancy,
    ar1_parameter_efficiency,
    scale_parameter
  ){
    unemployed <- 
      exp(
        arima.sim(
          list(
            order = c(1,0,0), # ar 1
            ar = ar1_parameter_unemployed
          ), 
          n = length(time)
        )
      ) * scale_parameter
    
    vacancy <- 
      exp(
        arima.sim(
          list(
            order = c(1,0,0), # ar 1
            ar = ar1_parameter_vacancy
          ), 
          n = length(time)
        )
      ) * scale_parameter
    
    efficiency <- 
      exp(
        arima.sim(
          list(
            order = c(1,0,0), # ar 1
            ar = ar1_parameter_efficiency
          ), 
          n = length(time)
        )
      )
    normalized_efficiency <-
      (
        efficiency/
          quantile(
            efficiency, 
            probs = 0.50
            ) 
      )/100
    data <-
      cbind(
        time,
        unemployed,
        vacancy,
        efficiency,
        normalized_efficiency
      ) %>% 
      tibble::as_tibble() %>% 
      dplyr::mutate(
        hire =
          # (normalized_efficiency *
          # unemployed)^hire_parameter*
          # vacancy^(1 - hire_parameter)
          hire_parameter * 
          normalized_efficiency *
          #efficiency *
          unemployed +
          hire_parameter *
          vacancy
      ) %>% 
      dplyr::mutate(
        overhire_check =
          (unemployed <= hire) |
          (vacancy <= hire)
      )
    return(data)
  }

generate_data <-
  function(
    time,
    list_simulation,
    ar1_parameter_unemployed,
    ar1_parameter_vacancy,
    ar1_parameter_efficiency,
    scale_parameter
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
        generate_ar1_data(
          time,
          ar1_parameter_unemployed,
          ar1_parameter_vacancy,
          ar1_parameter_efficiency,
          scale_parameter
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
  temp_nn <-
    list_num_time[nn]
  time <-
    c(1:temp_nn)
  data <-
    generate_data(
      time,
      list_simulation,
      ar1_parameter_unemployed,
      ar1_parameter_vacancy,
      ar1_parameter_efficiency,
      scale_parameter
    )
  filename <-
    paste(
      "output/matching_function_monte_carlo_data_",
      "num_time_",
      temp_nn,
      ".rds",
      sep = ""
    )
  cat(filename,"\n")
  saveRDS(
    data,
    file = filename)
}
