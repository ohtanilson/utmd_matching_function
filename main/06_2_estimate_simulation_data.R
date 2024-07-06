rm(list = ls())
library(magrittr)
# set constant ----

num_simulation <-
  100
list_num_time <-
  c(
    #10,
    #20,
    #30,
    #40,
    50,
    100,
    200
  )
## set stationarity parameter ----
arima_list <-
  list(
    "AR1_I0" = c(1,0,0),
    #"AR1_I1" = c(1,1,0),
    "AR0_I1" = c(0,1,0)
  )
maxA <-
  1000  # Maximum matching efficiency in grid (normalized at median vacancies to 100)
minA <-
  1   # Minimum matching efficiency in grid (normalized at median vacancies to 100)
kernel_sd <-
  0.1
Astar <-
  100
supportsize_theta <-
  100
## set matching function specification ----
specification_list <-
  c(
    "cobb_douglas",
    "perfect_substitute"#,
    #"fixed_proportion"
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
## set function ----

estimate_distribution_efficiency_conditional_on_unemployed <-
  function(
    data,
    supporttheta,
    supportlambda,
    Hstar,
    vstar,
    Ustar,
    hsvv,
    hsu
  ){
    FAS1 <- 
      matrix(
        0, 
        nrow = length(supporttheta), 
        ncol = length(supportlambda)
      )  
    for (il in 1:length(supportlambda)) {  # loop over the handle lambda over the S values
      lambda <- 
        supportlambda[il]
      for (it in 1:length(supporttheta)) {  # loop over handle theta corresponding to A
        theta <- 
          supporttheta[it]

        y <- 
          as.numeric(
            data$hire <= theta * lambda * Hstar
          )  
        # This does both Step 2 and 3
        # find G(H|U(i),v(j)) -- for each pair (U',v') we want to estimate the distribution over matches
        # below some value H. To do this, we average observed proportions
        # of H below this value across values of (U,v) that are close to
        # (U',v') -- weighted by a kernel to put more weight on close
        # values.
        kernel_input <-
          cbind(
            ((data$vacancy - theta * lambda * vstar) / hsvv),
            ((data$unemployed - lambda * Ustar) / hsu)
          )
        kernel_output <-
          mvtnorm::dmvnorm(kernel_input)
        FAS1[it, il] <- 
          sum(y * kernel_output) / 
          sum(kernel_output)

      }
    }
    return(FAS1)
  }
predict_efficiency <-
  function(
    data,
    hsvv,
    hsu,
    supportU,
    supportA,
    FAS1
  ){
    efficiency_implied <- 
      numeric(length(data$hire))  # aggregate matching efficiency
    for (i in 1:length(data$hire)) {
      y <- 
        as.numeric(
          data$hire <= data$hire[i]
        )
      # find G_H|u(i),v(i) at point H(i)
      kernel_input <-
        cbind(
          ((data$vacancy - data$vacancy[i]) / hsvv),
          ((data$unemployed - data$unemployed[i]) / hsu)
        )
      kernel_output <-
        mvtnorm::dmvnorm(kernel_input)
      GHsv <- 
        sum(y * kernel_output) / 
        sum(kernel_output)  
      unemployed_index <- 
        which.min(abs(supportU - data$unemployed[i]))
      efficiency_index <- 
        which.min(abs(FAS1[, unemployed_index] - GHsv))
      efficiency_implied[i] <- 
        supportA[efficiency_index]
    }
    return(efficiency_implied)
  }

estimate_efficiency <-
  function(
    cross_sectional_normalization,
    data,
    Astar,
    maxA,
    minA,
    supportsize_theta,
    kernel_sd = kernel_sd
  ){
    # fix normalized point
    if(cross_sectional_normalization == "location"){
      vstar <- 
        # quantile(
        #   data$vacancy, 
        #   probs = 0.50
        # )
        #data$vacancy[1] # initial date
        as.numeric(data[data$year == "2013"& data$prefecture == "東京都","vacancy"][1,]) # 1972 Jan
    }else if(cross_sectional_normalization == "job category"){
      vstar <- 
        # quantile(
        #   data$vacancy, 
        #   probs = 0.50
        # )
        #data$vacancy[1] # initial date
        as.numeric(data[data$year == "2013" & data$job_kinds == "事務的職業","vacancy"][1,]) # 1972 Jan
    }else{
      vstar <- 
        # quantile(
        #   data$vacancy,
        #   probs = 0.50
        # )
        data$vacancy[1] # initial date
        #as.numeric(data[data$time == 1,"vacancy"]) # initial date
    }
    
    # Find the index of the closest value to vstar in v
    point_vstar <- 
      which.min(abs(data$vacancy - vstar))
    # Get corresponding values of m and S
    Hstar <- 
      data$hire[point_vstar]
    Ustar <- 
      data$unemployed[point_vstar]
    # Value of matching efficiency at normalization point
    # Astar <- 
    #   100
    # Define the support for lambda and theta
    supportlambda <- 
      data$unemployed / Ustar
    # supportsize_theta <-
    #   100  # Size of the support for theta
    hh <-
      kernel_sd  # Standard deviation of the kernel (as a percentage of the range)
    supporttheta <- 
      seq(
        minA / Astar, 
        maxA / Astar, 
        length.out = supportsize_theta
      )
    
    # Calculate support for A
    supportA <- 
      Astar * supporttheta
    
    # Calculate supportU
    supportU <- 
      Ustar * supportlambda #= data$unemployed
    
    # For kernel
    hsvv <- 
      hh * diff(range(data$vacancy))
    hsu <-
      hh *
      max(c(diff(range(data$unemployed)), 1))
    
    FAS1 <-
      estimate_distribution_efficiency_conditional_on_unemployed(
        data,
        supporttheta,
        supportlambda,
        Hstar,
        vstar,
        Ustar,
        hsvv,
        hsu
      )
    efficiency_implied <-
      predict_efficiency(
        data,
        hsvv,
        hsu,
        supportU,
        supportA,
        FAS1
      )
    # true_efficiency_normalized <-
    #   ts((data$efficiency/
    #         data$efficiency[point_vstar]) * 100
    #   )
    return(efficiency_implied)
  }

estimate_efficiency_all_industry <-
  function(
    list_industry_group_name,
    cross_sectional_normalization,
    data,
    Astar,
    maxA,
    minA,
    supportsize_theta,
    kernel_sd
  ){
    for(nn in 1:length(list_industry_group_name)){
      target_industry_group_name <-
        list_industry_group_name[nn]
      # month data
      target_data <-
        data %>% 
        dplyr::filter(
          industry_group_name == 
            target_industry_group_name
        ) 
      efficiency_implied <-
        estimate_efficiency(
          cross_sectional_normalization,
          target_data,
          Astar,
          maxA,
          minA,
          supportsize_theta,
          kernel_sd = kernel_sd
        )
      target_data <-
        cbind(
          target_data,
          efficiency_implied
        ) %>% 
        dplyr::mutate(
          efficiency_unemployed =
            efficiency_implied * 
            unemployed
        )
      # estimate match elasticity w.r.t. vacancy and unemployed
      # estimate match elasticity w.r.t. vacancy and unemployed
      lm_formula <-
        as.formula(
          paste(
            "hire ~", 
            "vacancy + efficiency_unemployed +",
            #"I(vacancy*efficiency_unemployed) + I(vacancy^2) + I(efficiency_unemployed^2)"
            "I(vacancy*efficiency_unemployed)"
          )
        )
      res <-
        lm(lm_formula,
           data = target_data)
      #summary(res)
      # dlog(H)/dlog(AU) = (AU/H)*(coef(AU) + coef(AU:V)V + 2*coef(AU^2)AU)
      hire_elasticity_efficiency_unemployed <-
        (res$coefficients["efficiency_unemployed"] +
           res$coefficients["I(vacancy * efficiency_unemployed)"] * target_data$vacancy# +
         #2 * res$coefficients["I(efficiency_unemployed^2)"] * target_data$efficiency_unemployed
        ) *
        (target_data$efficiency_unemployed/target_data$hire)
      # dlog(H)/dlog(V) = (V/H)*(coef(V) + coef(AU:V)AU + 2*coef(V^2)AU)
      hire_elasticity_vacancy <-
        (res$coefficients["vacancy"] +
           res$coefficients["I(vacancy * efficiency_unemployed)"] * target_data$efficiency_unemployed# +
         #2 * res$coefficients["I(vacancy^2)"] * target_data$vacancy
        ) *
        (target_data$vacancy/target_data$hire)
      target_data <-
        cbind(
          target_data,
          hire_elasticity_vacancy,
          hire_elasticity_efficiency_unemployed
        )
      
      if(nn == 1){
        utmd_output_month <-
          target_data
      }else{
        utmd_output_month <-
          rbind(
            utmd_output_month,
            target_data
          )
      }
      
    }
    return(utmd_output_month)
  }
assign_results <-
  function(
    target_data,
    cross_sectional_normalization
  ){
    temp_data <-
      estimate_efficiency_all_industry(
        list_industry_group_name = 
          unique(
            target_data$industry_group_name
          ),
        cross_sectional_normalization = 
          cross_sectional_normalization,
        data = target_data,
        Astar,
        maxA,
        minA,
        supportsize_theta,
        kernel_sd
      ) %>% 
      dplyr::rename(
        candidate_count = unemployed,  
        position_count = vacancy,   
        hire_count = hire
      )
    # test independence of AV conditional on U 
    residual_v_on_u <-
      lm(position_count ~ candidate_count,
         data = temp_data)$residual
    residual_a_on_u <-
      lm(efficiency_implied ~ candidate_count,
         data = temp_data)$residual
    temp_data <-
      cbind(
        temp_data,
        residual_v_on_u, 
        residual_a_on_u
      )
    # filename <-
    #   paste(
    #     "utmd_output_",
    #     deparse(substitute(target_data)),
    #     sep = "")
    # cat(filename, "\n")
    # assign(
    #   filename,
    #   temp_data
    # )
    return(temp_data)
  }


# load, estimate, and save data ----
for(nn in 1:length(list_num_time)){
  for(mm in 1:length(specification_list)){
    for(rr in 1:length(arima_list)){
      for(qq in 1:length(dependency_V_of_A_list)){
        dependency_V_of_A <-
          dependency_V_of_A_list[qq]
        target_arima <-
          arima_list[[rr]]
        target_arima_name <-
          names(arima_list)[rr]
        CRS_gamma_parameter <-
          0.3
        temp_nn <-
          list_num_time[nn]
        matching_function_specification <-
          specification_list[mm]
        temp_nn <-
          list_num_time[nn]
        filename <-
          paste(
            #"output/monte_carlo_data_",
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
            sep = ""
          )
        cat(filename,"\n")
        # load 
        target_data <-
          readRDS(
            file = 
              here::here(
                paste(
                  "output/",
                  "monte_carlo_data_",
                  filename,
                  ".rds",
                  sep = ""
                )
              )
          )
        # assign(filename,
        #        temp_data)
        # estimate 
        for(ss in 1:num_simulation){
          cat(ss,"\n")
          target_data_ss <-
            target_data %>% 
            dplyr::filter(
              simulation_id == ss
            )
          target_result <-
            assign_results(
              target_data = target_data_ss,
              cross_sectional_normalization = FALSE
            )
          if(ss == 1){
            target_data_merged <-
              target_result
          }else{
            target_data_merged <-
              rbind(
                target_data_merged,
                target_result
              )
          }
        }
        
        # save 
        saveRDS(
          target_data_merged,
          file = 
            paste(
              "output/",
              "implied_efficiency_",
              filename,
              ".rds",
              sep = ""
            )
        )
      }
      
      
    }
    
    
  }
}

