
rm(list = ls())
library(magrittr)

# load ----

## government data ----
hello_work_data <- 
  readxl::read_excel(
    here::here("input/一般職業紹介状況（職業安定業務統計）第1表.xlsx")
  )
hello_work_data <-
  hello_work_data[c(-1:-3,-483:-486),c(1,3,8:12)]
colnames(hello_work_data) <-
  c("year",
    "month",
    "unemployed_inflow",
    "vacancy_inflow",
    "unemployed",
    "vacancy",
    "hire")
hello_work_data_yearly <-
  hello_work_data %>% 
  dplyr::filter(
    is.na(month) == 1
  )
hello_work_data_yearly <-
  hello_work_data_yearly[1:61,]
hello_work_data <-
  hello_work_data %>% 
  dplyr::filter(
    is.na(month) != 1
  )
hello_work_data <-
  hello_work_data[c(-1:-89),]
hello_work_data <-
  hello_work_data %>% 
  dplyr::mutate(
    year =
      stringr::str_replace_all(
        year,
        "年", 
        ""
      ),
    month =
      dplyr::case_when(
        month == "１月" ~ "01-01",
        month == "２月" ~ "02-01",
        month == "３月" ~ "03-01",
        month == "４月" ~ "04-01",
        month == "５月" ~ "05-01",
        month == "６月" ~ "06-01",
        month == "７月" ~ "07-01",
        month == "８月" ~ "08-01",
        month == "９月" ~ "09-01",
        month == "10月" ~ "10-01",
        month == "11月" ~ "11-01",
        month == "12月" ~ "12-01"
      )
  ) %>% 
  dplyr::mutate(
    time = 
      paste(
        year,
        "-",
        month,
        sep = ""
      ),
    industry_group_name =
      "japan"
  ) %>% 
  dplyr::mutate(
    time = as.Date(time),
    unemployed_inflow = as.numeric(unemployed_inflow),
    vacancy_inflow = as.numeric(vacancy_inflow),
    unemployed = as.numeric(unemployed),
    vacancy = as.numeric(vacancy),
    hire = as.numeric(hire)
  )
hello_work_data_yearly <-
  hello_work_data_yearly %>% 
  dplyr::mutate(
    year =
      stringr::str_replace_all(
        year,
        "年", 
        ""
      )
  ) %>% 
  dplyr::mutate(
    time = year,
    industry_group_name =
      "japan"
  ) %>% 
  dplyr::mutate(
    unemployed_inflow = as.numeric(unemployed_inflow),
    vacancy_inflow = as.numeric(vacancy_inflow),
    unemployed = as.numeric(unemployed),
    vacancy = as.numeric(vacancy),
    hire = as.numeric(hire)
  )




# save ----
saveRDS(
  hello_work_data,
  file = here::here("cleaned/hello_work_data.rds")
)
saveRDS(
  hello_work_data_yearly,
  file = here::here("cleaned/hello_work_data_yearly.rds")
)
