
rm(list = ls())
library(magrittr)

# load ----
hello_work_data <-
  readRDS(file = here::here("cleaned/hello_work_data.rds"))
hello_work_data_yearly <-
  readRDS(file = here::here("cleaned/hello_work_data_yearly.rds"))
hello_work_data_full_time_monthly <-
  readRDS(file = here::here("cleaned/hello_work_data_full_time_monthly.rds"))
hello_work_data_part_time_monthly <-
  readRDS(file = here::here("cleaned/hello_work_data_part_time_monthly.rds"))
hello_work_data_part_and_full_time_monthly <-
  readRDS(file = here::here("cleaned/hello_work_data_part_and_full_time_monthly.rds"))
hello_work_data_monthly_prefecture <-
  readRDS(file = here::here("cleaned/hello_work_data_monthly_prefecture.rds")) %>% 
  dplyr::filter(
    prefecture != "全国"
  ) %>% 
  dplyr::select(
    year,
    month,
    year_month,
    type,
    unemployed_keep,
    vacancy_keep,
    hire,
    prefecture
  ) %>% 
  dplyr::rename(
    time = year_month,
    vacancy = vacancy_keep,
    unemployed = unemployed_keep
  ) %>% 
  dplyr::mutate(
    industry_group_name = "prefecture_level"
  )
hello_work_data_monthly_job_category <-
  readRDS(file = here::here("cleaned/hello_work_data_monthly_job_category.rds")) %>% 
  dplyr::filter(
    job_kinds != "職業計"
  ) %>% 
  dplyr::filter(
    job_kinds != "分類不能の職業"
  ) %>% 
  dplyr::select(
    year,
    month,
    year_month,
    type,
    unemployed_keep,
    vacancy_keep,
    hire,
    job_kinds
  ) %>% 
  dplyr::rename(
    time = year_month,
    vacancy = vacancy_keep,
    unemployed = unemployed_keep
  ) %>% 
  dplyr::mutate(
    industry_group_name = "job_category_level"
  )
## decompose ----
colnames(hello_work_data_part_and_full_time_monthly)
colnames(hello_work_data_monthly_prefecture)
colnames(hello_work_data_monthly_job_category)
### prefecture level ----
hello_work_data_full_time_monthly_prefecture <-
  hello_work_data_monthly_prefecture %>% 
  dplyr::filter(
    type == "full-time"
  )
hello_work_data_part_time_monthly_prefecture <-
  hello_work_data_monthly_prefecture %>% 
  dplyr::filter(
    type == "part-time"
  )
hello_work_data_part_and_full_time_monthly_prefecture <-
  hello_work_data_monthly_prefecture %>% 
  dplyr::filter(
    type == "both"
  )
### job category level ----
hello_work_data_full_time_monthly_job_category <-
  hello_work_data_monthly_job_category %>% 
  dplyr::filter(
    type == "full-time"
  )
hello_work_data_part_time_monthly_job_category <-
  hello_work_data_monthly_job_category %>% 
  dplyr::filter(
    type == "part-time"
  )
hello_work_data_part_and_full_time_monthly_job_category <-
  hello_work_data_monthly_job_category %>% 
  dplyr::filter(
    type == "both"
  )

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
        #data$vacancy[1] # initial date
        as.numeric(data[data$year == "1972","vacancy"][1,]) # 1972 Jan
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

# estimate ----

## government data ----
if(0 == 1){
  # for BizReach
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
}

target_result <-
  assign_results(
    target_data = hello_work_data_yearly,
    cross_sectional_normalization = FALSE
  )
assign(
  paste(
    "utmd_output_",
    deparse(substitute(hello_work_data_yearly)),
    sep = ""),
  target_result
)
## monthly data ----
### country data ----
target_result <-
  assign_results(
    target_data = hello_work_data_full_time_monthly,
    cross_sectional_normalization = FALSE
  )
assign(
  paste(
    "utmd_output_",
    deparse(substitute(hello_work_data_full_time_monthly)),
    sep = ""),
  target_result
)
target_result <-
  assign_results(
    target_data = hello_work_data_part_time_monthly,
    cross_sectional_normalization = FALSE
  )
assign(
  paste(
    "utmd_output_",
    deparse(substitute(hello_work_data_part_time_monthly)),
    sep = ""),
  target_result
)
target_result <-
  assign_results(
    target_data = hello_work_data_part_and_full_time_monthly,
    cross_sectional_normalization = FALSE
  )
assign(
  paste(
    "utmd_output_",
    deparse(substitute(hello_work_data_part_and_full_time_monthly)),
    sep = ""),
  target_result
)
### prefecture data ----
system.time(
  target_result <-
    assign_results(
      hello_work_data_full_time_monthly_prefecture,
      cross_sectional_normalization = "location"
    )
)
assign(
  paste(
    "utmd_output_",
    deparse(substitute(hello_work_data_full_time_monthly_prefecture)),
    sep = ""),
  target_result
)
system.time(
  target_result <-
    assign_results(
      hello_work_data_part_time_monthly_prefecture,
      cross_sectional_normalization = "location"
    )
)
assign(
  paste(
    "utmd_output_",
    deparse(substitute(hello_work_data_part_time_monthly_prefecture)),
    sep = ""),
  target_result
)
system.time(
  target_result <-
    assign_results(
      hello_work_data_part_and_full_time_monthly_prefecture,
      cross_sectional_normalization = "location"
    )
)
assign(
  paste(
    "utmd_output_",
    deparse(substitute(hello_work_data_part_and_full_time_monthly_prefecture)),
    sep = ""),
  target_result
)



 
### job category data ----
system.time(
  target_result <-
    assign_results(
      hello_work_data_full_time_monthly_job_category,
      cross_sectional_normalization = "job category"
    )
)
assign(
  paste(
    "utmd_output_",
    deparse(substitute(hello_work_data_full_time_monthly_job_category)),
    sep = ""),
  target_result
)

system.time(
  target_result <-
    assign_results(
      hello_work_data_part_time_monthly_job_category,
      cross_sectional_normalization = "job category"
    )
)
assign(
  paste(
    "utmd_output_",
    deparse(substitute(hello_work_data_part_time_monthly_job_category)),
    sep = ""),
  target_result
)

system.time(
  target_result <-
    assign_results(
      hello_work_data_part_and_full_time_monthly_job_category,
      cross_sectional_normalization = "job category"
    )
)
assign(
  paste(
    "utmd_output_",
    deparse(substitute(hello_work_data_part_and_full_time_monthly_job_category)),
    sep = ""),
  target_result
)


# save ----

# saveRDS(
#   utmd_output_hello_work_data,
#   file = here::here("output/utmd_output_hello_work_data.rds")
# )
saveRDS(
  utmd_output_hello_work_data_yearly,
  file = here::here("output/utmd_output_hello_work_data_yearly.rds")
)
## monthly data ----
### country data ----
saveRDS(
  utmd_output_hello_work_data_full_time_monthly,
  file = here::here("output/utmd_output_hello_work_data_full_time_monthly.rds")
)
saveRDS(
  utmd_output_hello_work_data_part_time_monthly,
  file = here::here("output/utmd_output_hello_work_data_part_time_monthly.rds")
)
saveRDS(
  utmd_output_hello_work_data_part_and_full_time_monthly,
  file = here::here("output/utmd_output_hello_work_data_part_and_full_time_monthly.rds")
)
### prefecture data ----
saveRDS(
  utmd_output_hello_work_data_part_time_monthly_prefecture,
  file = here::here("output/utmd_output_hello_work_data_part_time_monthly_prefecture.rds")
)
saveRDS(
  utmd_output_hello_work_data_full_time_monthly_prefecture,
  file = here::here("output/utmd_output_hello_work_data_full_time_monthly_prefecture.rds")
)
saveRDS(
  utmd_output_hello_work_data_part_and_full_time_monthly_prefecture,
  file = here::here("output/utmd_output_hello_work_data_part_and_full_time_monthly_prefecture.rds")
)




### job category data ----
saveRDS(
  utmd_output_hello_work_data_part_time_monthly_job_category,
  file = here::here("output/utmd_output_hello_work_data_part_time_monthly_job_category.rds")
)
saveRDS(
  utmd_output_hello_work_data_full_time_monthly_job_category,
  file = here::here("output/utmd_output_hello_work_data_full_time_monthly_job_category.rds")
)
saveRDS(
  utmd_output_hello_work_data_part_and_full_time_monthly_job_category,
  file = here::here("output/utmd_output_hello_work_data_part_and_full_time_monthly_job_category.rds")
)

