rm(list = ls())
library(magrittr)

# load data ----
# set constant ----
# monte carlo simulation to get nice sample size identifying match effect
num_time <-
  50
num_simulation <-
  100
num_time_list <-
  c(
    10,
    20,
    30,
    40,
    50,
    100,
    500
  )
list_time_id <-
  c(1:num_time)
list_simulation <-
  c(1:num_simulation)
set.seed(1)



# generate data ----

generate_data <-
  function(
    list_horse_id,
    list_category_id,
    list_time,
    list_simulation
  ){
    data <-
      expand.grid(
        list_horse_id,
        list_time,
        list_simulation
      ) %>% 
      tibble::as_tibble() 
    data$error <-
      rnorm(dim(data)[1])
    colnames(data) <-
      c(
        "horse_id",
        "time_id",
        "simulation_id",
        "error"
      )
    data <-
      data %>% 
      dplyr::group_by(
        horse_id,
        category_id
      ) %>% 
      dplyr::mutate(
        match_effect =
          #rnorm(1) + 10
          horse_id*category_id/3
      ) %>% 
      dplyr::ungroup()
    
    data <-
      data %>% 
      dplyr::mutate(
        y = 
          horse_id + 
          category_id + 
          match_effect +
          error
      )
    return(data)
  }



# save data ----
# saveRDS(
#   match_effect_monte_carlo_data, 
#   file = "output/match_effect_monte_carlo_data.rds"
# )