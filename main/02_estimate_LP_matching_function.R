
rm(list = ls())
library(magrittr)

# load ----
hello_work_data <-
  readRDS(file = here::here("cleaned/hello_work_data.rds"))
hello_work_data_yearly <-
  readRDS(file = here::here("cleaned/hello_work_data_yearly.rds"))

# set constant ----
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
# set function ----

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
    kernel_sd = kernel_sd
  ){
    # fix normalized point
    vstar <- 
      # quantile(
      #   data$vacancy, 
      #   probs = 0.50
      # )
      #data$vacancy[1] # initial date
      as.numeric(data[data$year == "2002","vacancy"][1,]) # 2002 Jan
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
      res <-
        lm(hire ~ vacancy + efficiency_unemployed,
           data = target_data)
      hire_elasticity_efficiency_unemployed <-
        res$coefficients["efficiency_unemployed"] *
        (target_data$efficiency_unemployed/target_data$hire)
      hire_elasticity_unemployed <-
        res$coefficients["efficiency_unemployed"] *
        target_data$efficiency_implied *
        (target_data$efficiency_unemployed/target_data$hire)
      hire_elasticity_vacancy <-
        res$coefficients["vacancy"] *
        (target_data$vacancy/target_data$hire)
      target_data <-
        cbind(
          target_data,
          hire_elasticity_vacancy,
          hire_elasticity_unemployed,
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



# estimate ----

## government data ----

utmd_output_hello_work_data <-
  estimate_efficiency_all_industry(
    list_industry_group_name = 
      unique(
        hello_work_data$industry_group_name
        ),
    data = hello_work_data,
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



utmd_output_hello_work_data_yearly <-
  estimate_efficiency_all_industry(
    list_industry_group_name = 
      unique(
        hello_work_data_yearly$industry_group_name
      ),
    data = hello_work_data_yearly,
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


# save ----

saveRDS(
  utmd_output_hello_work_data,
  file = here::here("output/utmd_output_hello_work_data.rds")
)
saveRDS(
  utmd_output_hello_work_data_yearly,
  file = here::here("output/utmd_output_hello_work_data_yearly.rds")
)