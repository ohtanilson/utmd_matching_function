
rm(list = ls())
library(magrittr)
# set constant ----
# monte carlo simulation to get nice sample size identifying efficiency
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
maxA <-
  300  # Maximum matching efficiency in grid (normalized at median vacancies to 100)
minA <-
  10   # Minimum matching efficiency in grid (normalized at median vacancies to 100)


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
    data,
    Astar,
    maxA,
    minA,
    supportsize_theta,
    kernel_sd = 0.1
    ){
    vstar <- 
      quantile(
        data$vacancy, 
        probs = 0.50
      )
    
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



# load, estimate, compute bias, save ----
## benchmark data ----
for(nn in 1:length(list_num_time)){
  temp_nn <-
    list_num_time[nn]
  filename <-
    paste(
      "output/matching_function_monte_carlo_data_",
      "num_time_",
      temp_nn,
      #".rds",
      sep = ""
    )
  cat(filename,"\n")
  # load 
  target_data <-
    readRDS(
      file = 
        here::here(
          paste(
            #"output/",
            filename,
            ".rds",
            sep = ""
          )
        )
    )
  # estimate 
  efficiency_implied <-
    estimate_efficiency(
      target_data %>% 
        dplyr::filter(simulation_id == 1),
      Astar = 100,
      maxA,
      minA,
      supportsize_theta = 100,
      kernel_sd = 0.1
    )
  # res <-
  #   target_data %>% 
  #   split(
  #     .$simulation_id
  #   ) %>% 
  #   purrr::map(
  #     ~ lfe::felm(
  #       formula = 
  #         as.formula(
  #           paste(
  #             "y ~", 
  #             "factor(worker_id)*factor(category_id)",
  #             "|",
  #             "0|",
  #             "0|",
  #             "0"
  #           )
  #         ),
  #       data = .x)
  #   ) %>% 
  #   purrr::map(summary)
  # 
  # for(ss in 1:num_simulation){
  #   bias <-
  #     compute_bias(
  #       target_data,
  #       target_simulation_id = ss,
  #       target_estimated_coefficients =
  #         as.numeric(res[[ss]]$coefficients[,1])
  #     )
  #   t_stats <-
  #     res[ss][[1]]$coefficients[,3]
  #   beta_hat <-
  #     res[ss][[1]]$coefficients[,1]
  #   if(ss == 1){
  #     bias_table <-
  #       bias
  #     t_stats_table <-
  #       t_stats
  #     beta_hat_table <-
  #       beta_hat
  #   }else{
  #     bias_table <-
  #       rbind(
  #         bias_table,
  #         bias
  #       ) %>% 
  #       tibble::as_tibble()
  #     t_stats_table <-
  #       rbind(
  #         t_stats_table,
  #         t_stats
  #       ) %>% 
  #       tibble::as_tibble()
  #     beta_hat_table <-
  #       rbind(
  #         beta_hat_table,
  #         beta_hat
  #       ) %>% 
  #       tibble::as_tibble()
  #   }
  # }
  # colnames(bias_table) <-
  #   rownames(res[[ss]]$coefficients)
  # colnames(t_stats_table) <-
  #   rownames(res[[ss]]$coefficients)
  # colnames(beta_hat_table) <-
  #   rownames(res[[ss]]$coefficients)
  # # save 
  # saveRDS(
  #   bias_table,
  #   file = 
  #     paste(
  #       "output/",
  #       "bias_table_",
  #       "num_process_",
  #       temp_nn,
  #       ".rds",
  #       sep = ""
  #     )
  # )
  # saveRDS(
  #   t_stats_table,
  #   file = 
  #     paste(
  #       "output/",
  #       "t_stats_table_",
  #       "num_process_",
  #       temp_nn,
  #       ".rds",
  #       sep = ""
  #     )
  # )
  # saveRDS(
  #   beta_hat_table,
  #   file = 
  #     paste(
  #       "output/",
  #       "beta_hat_table_",
  #       "num_process_",
  #       temp_nn,
  #       ".rds",
  #       sep = ""
  #     )
  # )
}