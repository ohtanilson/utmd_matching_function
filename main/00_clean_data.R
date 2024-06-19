
rm(list = ls())
library(magrittr)

# load ----

## government data ----
### yearly data ----
#`労働市場関係指標（パートタイムを含む一般）`
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
### monthly data ----
col.label <- 
  c("year",
    "1",
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "8",
    "9",
    "10",
    "11",
    "12",
    "type",
    "group")

select.raw <- 14:63

select.column <- c(1,3:14)

raw.vacancy.full <-
  here::here("input/第6表.xlsx") %>% 
  readxl::read_excel(sheet = "第６表ー２（パート除く）") %>%
  .[select.raw,select.column] %>% 
  dplyr::mutate(type = "求人",
         group = "フルタイム")

colnames(raw.vacancy.full) <- col.label

raw.seeker.full <-
  here::here("input/第7表.xlsx") %>% 
  readxl::read_excel(sheet = "第７表ー２（パート除く）") %>%
  .[select.raw,select.column] %>% 
  dplyr::mutate(type = "求職",
         group = "フルタイム")

colnames(raw.seeker.full) <- col.label

raw.hir.full <-
  here::here("input/第8表.xlsx") %>% 
  readxl::read_excel(sheet = "第８表ー２（パート除く）") %>%
  .[select.raw,select.column] %>% 
  dplyr::mutate(type = "新規就職",
         group = "フルタイム")

colnames(raw.hir.full) <- col.label

raw.vacancy.part <-
  here::here("input/第6表.xlsx") %>% 
  readxl::read_excel(sheet = "第６表ー３（パート）") %>%
  .[select.raw,select.column] %>% 
  dplyr::mutate(type = "求人",
         group = "パートタイム")

colnames(raw.vacancy.part) <- col.label

raw.seeker.part <-
  here::here("input/第7表.xlsx") %>% 
  readxl::read_excel(sheet = "第７表ー３（パート）")  %>%
  .[select.raw,select.column] %>% 
  dplyr::mutate(type = "求職",
         group = "パートタイム")

colnames(raw.seeker.part) <- col.label

raw.hir.part <-
  here::here("input/第8表.xlsx") %>% 
  readxl::read_excel(sheet = "第８表ー３（パート）") %>%
  .[select.raw,select.column] %>% 
  dplyr::mutate(type = "新規就職",
         group = "パートタイム")

colnames(raw.hir.part) <- col.label

df <-
  rbind(raw.hir.full,
        raw.hir.part,
        raw.vacancy.full,
        raw.vacancy.part,
        raw.seeker.full,
        raw.seeker.part
  ) %>% 
  tidyr::pivot_longer(
    cols = 2:13,
    names_to = "month",
    values_to = "n"
    ) %>% 
  dplyr::mutate(n = n %>% as.numeric(),
         year = year %>% stringr::str_sub(1,4) %>% as.numeric(),
         month = month %>% as.numeric(),
         quaterly = month %>% cut(c(0,3,6,9,12), labels = c(1,2,3,4)),
         time = lubridate::ym(stringr::str_c(year,month))
  )
#### full_time_monthly ----
helloworker_data_full_time_monthly <-
  df %>% 
  dplyr::filter(
    group == "フルタイム"
  )
unemployed <-
  helloworker_data_full_time_monthly %>% 
  dplyr::filter(
    type == "求職"
  ) %>% 
  dplyr::select(n) %>% 
  dplyr::rename(
    unemployed = n
  )
vacancy <-
  helloworker_data_full_time_monthly %>% 
  dplyr::filter(
    type == "求人"
  ) %>% 
  dplyr::select(n) %>% 
  dplyr::rename(
    vacancy = n
  )
hire <-
  helloworker_data_full_time_monthly %>% 
  dplyr::filter(
    type == "新規就職"
  ) %>% 
  dplyr::select(n) %>% 
  dplyr::rename(
    hire = n
  )
helloworker_data_full_time_monthly <-
  helloworker_data_full_time_monthly %>% 
  dplyr::filter(
    type == "新規就職"
  ) %>% 
  dplyr::distinct(
    year,
    month,
    time
  ) %>% 
  dplyr::mutate(
    unemployed,
    vacancy,
    hire,
    industry_group_name =
      "japan"
  )
#### part_time_monthly ----
helloworker_data_part_time_monthly <-
  df %>% 
  dplyr::filter(
    group == "パートタイム"
  )
unemployed <-
  helloworker_data_part_time_monthly %>% 
  dplyr::filter(
    type == "求職"
  ) %>% 
  dplyr::select(n) %>% 
  dplyr::rename(
    unemployed = n
  )
vacancy <-
  helloworker_data_part_time_monthly %>% 
  dplyr::filter(
    type == "求人"
  ) %>% 
  dplyr::select(n) %>% 
  dplyr::rename(
    vacancy = n
  )
hire <-
  helloworker_data_part_time_monthly %>% 
  dplyr::filter(
    type == "新規就職"
  ) %>% 
  dplyr::select(n) %>% 
  dplyr::rename(
    hire = n
  )

helloworker_data_part_time_monthly <-
  helloworker_data_part_time_monthly %>% 
  dplyr::filter(
    type == "新規就職"
  ) %>% 
  dplyr::distinct(
    year,
    month,
    time
  ) %>% 
  dplyr::mutate(
    unemployed,
    vacancy,
    hire,
    industry_group_name =
      "japan"
  )
helloworker_data_part_and_full_time_monthly <-
  helloworker_data_part_time_monthly %>% 
  dplyr::mutate(
    unemployed = 
      unemployed +
      helloworker_data_full_time_monthly$unemployed,
    vacancy =
      vacancy +
      helloworker_data_full_time_monthly$vacancy,
    hire = 
      hire +
      helloworker_data_full_time_monthly$hire
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
saveRDS(
  helloworker_data_full_time_monthly,
  file = here::here("cleaned/helloworker_data_full_time_monthly.rds")
)
saveRDS(
  helloworker_data_part_time_monthly,
  file = here::here("cleaned/helloworker_data_part_time_monthly.rds")
)
saveRDS(
  helloworker_data_part_and_full_time_monthly,
  file = here::here("cleaned/helloworker_data_part_and_full_time_monthly.rds")
)

