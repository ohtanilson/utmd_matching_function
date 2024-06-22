# load ----
## packages reading
rm(list = ls())
#install.packages("pacman")
pacman::p_load(
  tidyverse,
  readxl,
  lubridate, ## 日付を処理するパッケージ
  plotly ## HTMLでggplotグラフをきれいに見るパッケージ
)

## 都道府県別 ----
## 受理地
sheet_names <- excel_sheets("input/第11表.xlsx")

list_zyurichi <- list()

## 求人倍率系のデータは必要ないので、それはデータ整形しない。
set_diff_vec <- setdiff(1:length(sheet_names), c(5, 6, 11, 12))

## データの形も違う

for(i in set_diff_vec){
  print(i)
  ## data reading
  data_i <- read_xlsx("input/第11表.xlsx",
                      sheet = i,
                      skip = 1
  ) %>%
    rename(month = ...3) %>%
    drop_na(month) %>% ## 年次の月平均データなどを落としている
    mutate( ## yearとmonthを整備
      year = str_remove(string = 西暦, pattern = "年"),
      year = str_remove(string = year, pattern = "度"),
      year = as.numeric(year),
      month = case_when(
        month == "１月" ~ 1,
        month == "２月" ~ 2,
        month == "３月" ~ 3,
        month == "４月" ~ 4,
        month == "５月" ~ 5,
        month == "６月" ~ 6,
        month == "７月" ~ 7,
        month == "８月" ~ 8,
        month == "９月" ~ 9,
        month == "10月" ~ 10,
        month == "11月" ~ 11,
        month == "12月" ~ 12
      )
    ) %>%
    pivot_longer(
      cols = "全国":"沖縄県",
      values_to = "number",
      names_to = "prefecture"
    ) %>%
    mutate(
      year_month = make_date(year = year, month = month, day = 1),
      data_type = sheet_names[i]) %>%
    select(prefecture, 
           data_type,
           year_month,
           year,
           month,
           number)
  
  
  list_zyurichi[[i]] <- data_i
}


## listの中に入っているデータをleft_joinでつなげる
data_long <- list_zyurichi[[1]]

for(i in setdiff(set_diff_vec, 1)){
  data_long <- data_long %>%
    bind_rows(list_zyurichi[[i]])
}

## 単に、データの種類でwide型にする
prefecture_data <- data_long %>%
  filter(str_detect(string = data_type, pattern = "実数")) %>%
  mutate(
    data_type = case_when(
      str_detect(string = data_type, pattern = "新規求人") ~ "vacancy_new",
      str_detect(string = data_type, pattern = "新規求職") ~ "unemployed_new",
      str_detect(string = data_type, pattern = "有効求人") ~ "vacancy_keep",
      str_detect(string = data_type, pattern = "有効求職") ~ "unemployed_keep",
    )
  ) %>%
  pivot_wider(
    names_from = "data_type",
    values_from = "number"
  )

## 就業地（求人データは、就業地のほうがその県の現状を反映している）
sheet_names <- excel_sheets("input/第18表.xlsx")

list_syugyochi <- list()

## 求人倍率系のデータは必要ないので、それはデータ整形しない。
set_diff_vec <- 1:2 ## sheet3移行のデータは、求人倍率と季節調整値


for(i in set_diff_vec){
  print(i)
  ## data reading
  data_i <- read_xlsx("input/第18表.xlsx",
                      sheet = i,
                      skip = 1
  ) %>%
    rename(month = ...3) %>%
    drop_na(month) %>% ## 年次の月平均データなどを落としている
    mutate( ## yearとmonthを整備
      year = str_remove(string = 西暦, pattern = "年"),
      year = str_remove(string = year, pattern = "度"),
      year = as.numeric(year),
      month = case_when(
        month == "１月" ~ 1,
        month == "２月" ~ 2,
        month == "３月" ~ 3,
        month == "４月" ~ 4,
        month == "５月" ~ 5,
        month == "６月" ~ 6,
        month == "７月" ~ 7,
        month == "８月" ~ 8,
        month == "９月" ~ 9,
        month == "10月" ~ 10,
        month == "11月" ~ 11,
        month == "12月" ~ 12
      )
    ) %>%
    pivot_longer(
      cols = "北海道":"沖縄県",
      values_to = "number",
      names_to = "prefecture"
    ) %>%
    mutate(
      year_month = make_date(year = year, month = month, day = 1),
      data_type = sheet_names[i]) %>%
    select(prefecture, 
           data_type,
           year_month,
           year,
           month,
           number)
  
  
  list_syugyochi[[i]] <- data_i
}


## listの中に入っているデータをleft_joinでつなげる
data_long <- list_syugyochi[[1]]

data_long <- data_long %>%
  bind_rows(list_syugyochi[[2]])


## 単に、データの種類でwide型にする
prefecture_data_syugyochi <- data_long %>%
  mutate(
    data_type = case_when(
      str_detect(string = data_type, pattern = "新規求人") ~ "vacancy_new_shugyo",
      str_detect(string = data_type, pattern = "有効求人") ~ "vacancy_keep_shugyo",
    )
  ) %>%
  pivot_wider(
    names_from = "data_type",
    values_from = "number"
  )

prefecture_data <- prefecture_data %>%
  left_join(prefecture_data_syugyochi,
            by = c("prefecture", 
                   "year_month",
                   "year",
                   "month"))

## 雇用形態別のものと接続するために、変数を作成
prefecture_data <- prefecture_data %>%
  mutate(
    type = "both",
    contract_type = "both"
  )

## データを数値にする
vec_names <- prefecture_data %>% names()
vec_names <- setdiff(vec_names, c("prefecture", "year_month", "type", "contract_type"))

for(i in vec_names){
  eval(
    parse(
      text = paste0("prefecture_data <- prefecture_data %>% mutate(", i," = as.numeric(", i,"))")
    )
  )
}

## 接続できているか確認
prefecture_data_hokkaido <- prefecture_data %>%
  filter(prefecture == "北海道")

## 雇用形態別 ----
# 新規求人
## 受理地
sheet_names <- excel_sheets("input/第12表.xlsx")

list_vacancy_new <- list()

## 求人倍率系のデータは必要ないので、それはデータ整形しない。
## データの形も違う

for(i in 1:length(sheet_names)){
  print(i)
  ## data reading
  data_i <- read_xlsx("input/第12表.xlsx",
                      sheet = i,
                      skip = 1
  ) %>%
    rename(month = ...3) %>%
    drop_na(month) %>% ## 年次の月平均データなどを落としている
    mutate( ## yearとmonthを整備
      year = case_when(
        西暦 == "2013年" ~ 2013,
        西暦 == "2014年" ~ 2014,
        西暦 == "2015年" ~ 2015,
        西暦 == "2016年" ~ 2016,
        西暦 == "2017年" ~ 2017,
        西暦 == "2018年" ~ 2018,
        西暦 == "2019年" ~ 2019,
        西暦 == "2020年" ~ 2020,
        西暦 == "2021年" ~ 2021,
        西暦 == "2022年" ~ 2022,
        西暦 == "2023年" ~ 2023,
        西暦 == "2024年" ~ 2024
      ),
      month = case_when(
        month == "１月" ~ 1,
        month == "２月" ~ 2,
        month == "３月" ~ 3,
        month == "４月" ~ 4,
        month == "５月" ~ 5,
        month == "６月" ~ 6,
        month == "７月" ~ 7,
        month == "８月" ~ 8,
        month == "９月" ~ 9,
        month == "10月" ~ 10,
        month == "11月" ~ 11,
        month == "12月" ~ 12
      )
    ) %>%
    pivot_longer(
      cols = "全国":"沖縄県",
      values_to = "vacancy_new",
      names_to = "prefecture"
    ) %>%
    mutate(
      year_month = make_date(year = year, month = month, day = 1),
      data_type = sheet_names[i]) %>%
    select(prefecture, 
           data_type,
           year_month,
           year,
           month,
           vacancy_new)
  
  
  list_vacancy_new[[i]] <- data_i
}


## listの中に入っているデータをleft_joinでつなげる
data_long <- list_vacancy_new[[1]]

for(i in setdiff(1:length(sheet_names), 1)){
  data_long <- data_long %>%
    bind_rows(list_vacancy_new[[i]])
}

## パートを含むかどうかで分ける
data_long <- data_long %>%
  mutate(type = case_when(
    str_detect(data_type, pattern = "パート含む一般") ~ "both",
    str_detect(data_type, pattern = "パート除く一般") ~ "full-time",
    str_detect(data_type, pattern = "一般パート") ~ "part-time",
    str_detect(data_type, pattern = "パート含む常用") ~ "both",
    str_detect(data_type, pattern = "パート除く常用") ~ "full-time",
    str_detect(data_type, pattern = "常用的パート") ~ "part-time",
  ),
  contract_type = case_when(
    str_detect(data_type, pattern = "パート含む一般") ~ "both",
    str_detect(data_type, pattern = "パート除く一般") ~ "both",
    str_detect(data_type, pattern = "一般パート") ~ "both",
    str_detect(data_type, pattern = "パート含む常用") ~ "long",
    str_detect(data_type, pattern = "パート除く常用") ~ "long",
    str_detect(data_type, pattern = "常用的パート") ~ "long",
  )
  )

vacancy_new <- data_long %>%
  select(-data_type) 
# 新規求職
## 受理地
sheet_names <- excel_sheets("input/第13表.xlsx")

list_unemployed_new <- list()

## 求人倍率系のデータは必要ないので、それはデータ整形しない。
## データの形も違う

for(i in 1:length(sheet_names)){
  print(i)
  ## data reading
  data_i <- read_xlsx("input/第13表.xlsx",
                      sheet = i,
                      skip = 1
  ) %>%
    rename(month = ...3) %>%
    drop_na(month) %>% ## 年次の月平均データなどを落としている
    mutate( ## yearとmonthを整備
      year = case_when(
        西暦 == "2013年" ~ 2013,
        西暦 == "2014年" ~ 2014,
        西暦 == "2015年" ~ 2015,
        西暦 == "2016年" ~ 2016,
        西暦 == "2017年" ~ 2017,
        西暦 == "2018年" ~ 2018,
        西暦 == "2019年" ~ 2019,
        西暦 == "2020年" ~ 2020,
        西暦 == "2021年" ~ 2021,
        西暦 == "2022年" ~ 2022,
        西暦 == "2023年" ~ 2023,
        西暦 == "2024年" ~ 2024
      ),
      month = case_when(
        month == "１月" ~ 1,
        month == "２月" ~ 2,
        month == "３月" ~ 3,
        month == "４月" ~ 4,
        month == "５月" ~ 5,
        month == "６月" ~ 6,
        month == "７月" ~ 7,
        month == "８月" ~ 8,
        month == "９月" ~ 9,
        month == "10月" ~ 10,
        month == "11月" ~ 11,
        month == "12月" ~ 12
      )
    ) %>%
    pivot_longer(
      cols = "全国":"沖縄県",
      values_to = "unemployed_new",
      names_to = "prefecture"
    ) %>%
    mutate(
      year_month = make_date(year = year, month = month, day = 1),
      data_type = sheet_names[i]) %>%
    select(prefecture, 
           data_type,
           year_month,
           year,
           month,
           unemployed_new)
  
  
  list_unemployed_new[[i]] <- data_i
}


## listの中に入っているデータをleft_joinでつなげる
data_long <- list_unemployed_new[[1]]

for(i in setdiff(1:length(sheet_names), 1)){
  data_long <- data_long %>%
    bind_rows(list_unemployed_new[[i]])
}

## パートを含むかどうかで分ける
data_long <- data_long %>%
  mutate(type = case_when(
    str_detect(data_type, pattern = "パート含む一般") ~ "both",
    str_detect(data_type, pattern = "パート除く一般") ~ "full-time",
    str_detect(data_type, pattern = "一般パート") ~ "part-time",
    str_detect(data_type, pattern = "パート含む常用") ~ "both",
    str_detect(data_type, pattern = "パート除く常用") ~ "full-time",
    str_detect(data_type, pattern = "常用的パート") ~ "part-time",
  ),
  contract_type = case_when(
    str_detect(data_type, pattern = "パート含む一般") ~ "both",
    str_detect(data_type, pattern = "パート除く一般") ~ "both",
    str_detect(data_type, pattern = "一般パート") ~ "both",
    str_detect(data_type, pattern = "パート含む常用") ~ "long",
    str_detect(data_type, pattern = "パート除く常用") ~ "long",
    str_detect(data_type, pattern = "常用的パート") ~ "long",
  )
  )

unemployed_new <- data_long %>%
  select(-data_type) 

# 有効求人
## 受理地
sheet_names <- excel_sheets("input/第14表.xlsx")

list_vacancy_keep <- list()

## 求人倍率系のデータは必要ないので、それはデータ整形しない。
## データの形も違う

for(i in 1:length(sheet_names)){
  print(i)
  ## data reading
  data_i <- read_xlsx("input/第14表.xlsx",
                      sheet = i,
                      skip = 1
  ) %>%
    rename(month = ...3) %>%
    drop_na(month) %>% ## 年次の月平均データなどを落としている
    mutate( ## yearとmonthを整備
      year = case_when(
        西暦 == "2013年" ~ 2013,
        西暦 == "2014年" ~ 2014,
        西暦 == "2015年" ~ 2015,
        西暦 == "2016年" ~ 2016,
        西暦 == "2017年" ~ 2017,
        西暦 == "2018年" ~ 2018,
        西暦 == "2019年" ~ 2019,
        西暦 == "2020年" ~ 2020,
        西暦 == "2021年" ~ 2021,
        西暦 == "2022年" ~ 2022,
        西暦 == "2023年" ~ 2023,
        西暦 == "2024年" ~ 2024
      ),
      month = case_when(
        month == "１月" ~ 1,
        month == "２月" ~ 2,
        month == "３月" ~ 3,
        month == "４月" ~ 4,
        month == "５月" ~ 5,
        month == "６月" ~ 6,
        month == "７月" ~ 7,
        month == "８月" ~ 8,
        month == "９月" ~ 9,
        month == "10月" ~ 10,
        month == "11月" ~ 11,
        month == "12月" ~ 12
      )
    ) %>%
    pivot_longer(
      cols = "全国":"沖縄県",
      values_to = "vacancy_keep",
      names_to = "prefecture"
    ) %>%
    mutate(
      year_month = make_date(year = year, month = month, day = 1),
      data_type = sheet_names[i]) %>%
    select(prefecture, 
           data_type,
           year_month,
           year,
           month,
           vacancy_keep)
  
  
  list_vacancy_keep[[i]] <- data_i
}


## listの中に入っているデータをleft_joinでつなげる
data_long <- list_vacancy_keep[[1]]

for(i in setdiff(1:length(sheet_names), 1)){
  data_long <- data_long %>%
    bind_rows(list_vacancy_keep[[i]])
}

## パートを含むかどうかで分ける
data_long <- data_long %>%
  mutate(type = case_when(
    str_detect(data_type, pattern = "パート含む一般") ~ "both",
    str_detect(data_type, pattern = "パート除く一般") ~ "full-time",
    str_detect(data_type, pattern = "一般パート") ~ "part-time",
    str_detect(data_type, pattern = "パート含む常用") ~ "both",
    str_detect(data_type, pattern = "パート除く常用") ~ "full-time",
    str_detect(data_type, pattern = "常用的パート") ~ "part-time",
  ),
  contract_type = case_when(
    str_detect(data_type, pattern = "パート含む一般") ~ "both",
    str_detect(data_type, pattern = "パート除く一般") ~ "both",
    str_detect(data_type, pattern = "一般パート") ~ "both",
    str_detect(data_type, pattern = "パート含む常用") ~ "long",
    str_detect(data_type, pattern = "パート除く常用") ~ "long",
    str_detect(data_type, pattern = "常用的パート") ~ "long",
  )
  )

vacancy_keep <- data_long %>%
  select(-data_type) 

# 有効求職者数
## 受理地
sheet_names <- excel_sheets("input/第15表.xlsx")

list_unemployed_keep <- list()

## 求人倍率系のデータは必要ないので、それはデータ整形しない。
## データの形も違う

for(i in 1:length(sheet_names)){
  print(i)
  ## data reading
  data_i <- read_xlsx("input/第15表.xlsx",
                      sheet = i,
                      skip = 1
  ) %>%
    rename(month = ...3) %>%
    drop_na(month) %>% ## 年次の月平均データなどを落としている
    mutate( ## yearとmonthを整備
      year = case_when(
        西暦 == "2013年" ~ 2013,
        西暦 == "2014年" ~ 2014,
        西暦 == "2015年" ~ 2015,
        西暦 == "2016年" ~ 2016,
        西暦 == "2017年" ~ 2017,
        西暦 == "2018年" ~ 2018,
        西暦 == "2019年" ~ 2019,
        西暦 == "2020年" ~ 2020,
        西暦 == "2021年" ~ 2021,
        西暦 == "2022年" ~ 2022,
        西暦 == "2023年" ~ 2023,
        西暦 == "2024年" ~ 2024
      ),
      month = case_when(
        month == "１月" ~ 1,
        month == "２月" ~ 2,
        month == "３月" ~ 3,
        month == "４月" ~ 4,
        month == "５月" ~ 5,
        month == "６月" ~ 6,
        month == "７月" ~ 7,
        month == "８月" ~ 8,
        month == "９月" ~ 9,
        month == "10月" ~ 10,
        month == "11月" ~ 11,
        month == "12月" ~ 12
      )
    ) %>%
    pivot_longer(
      cols = "全国":"沖縄県",
      values_to = "unemployed_keep",
      names_to = "prefecture"
    ) %>%
    mutate(
      year_month = make_date(year = year, month = month, day = 1),
      data_type = sheet_names[i]) %>%
    select(prefecture, 
           data_type,
           year_month,
           year,
           month,
           unemployed_keep)
  
  
  list_unemployed_keep[[i]] <- data_i
}


## listの中に入っているデータをleft_joinでつなげる
data_long <- list_unemployed_keep[[1]]

for(i in setdiff(1:length(sheet_names), 1)){
  data_long <- data_long %>%
    bind_rows(list_unemployed_keep[[i]])
}

## パートを含むかどうかで分ける
data_long <- data_long %>%
  mutate(type = case_when(
    str_detect(data_type, pattern = "パート含む一般") ~ "both",
    str_detect(data_type, pattern = "パート除く一般") ~ "full-time",
    str_detect(data_type, pattern = "一般パート") ~ "part-time",
    str_detect(data_type, pattern = "パート含む常用") ~ "both",
    str_detect(data_type, pattern = "パート除く常用") ~ "full-time",
    str_detect(data_type, pattern = "常用的パート") ~ "part-time",
  ),
  contract_type = case_when(
    str_detect(data_type, pattern = "パート含む一般") ~ "both",
    str_detect(data_type, pattern = "パート除く一般") ~ "both",
    str_detect(data_type, pattern = "一般パート") ~ "both",
    str_detect(data_type, pattern = "パート含む常用") ~ "long",
    str_detect(data_type, pattern = "パート除く常用") ~ "long",
    str_detect(data_type, pattern = "常用的パート") ~ "long",
  )
  )

unemployed_keep <- data_long %>%
  select(-data_type) 

# 就職件数
## 受理地
sheet_names <- excel_sheets("input/第16表.xlsx")

list_hire <- list()

## 求人倍率系のデータは必要ないので、それはデータ整形しない。
## データの形も違う

for(i in 1:length(sheet_names)){
  print(i)
  ## data reading
  data_i <- read_xlsx("input/第16表.xlsx",
                      sheet = i,
                      skip = 1
  ) %>%
    rename(month = ...3) %>%
    drop_na(month) %>% ## 年次の月平均データなどを落としている
    mutate( ## yearとmonthを整備
      year = case_when(
        西暦 == "2013年" ~ 2013,
        西暦 == "2014年" ~ 2014,
        西暦 == "2015年" ~ 2015,
        西暦 == "2016年" ~ 2016,
        西暦 == "2017年" ~ 2017,
        西暦 == "2018年" ~ 2018,
        西暦 == "2019年" ~ 2019,
        西暦 == "2020年" ~ 2020,
        西暦 == "2021年" ~ 2021,
        西暦 == "2022年" ~ 2022,
        西暦 == "2023年" ~ 2023,
        西暦 == "2024年" ~ 2024
      ),
      month = case_when(
        month == "１月" ~ 1,
        month == "２月" ~ 2,
        month == "３月" ~ 3,
        month == "４月" ~ 4,
        month == "５月" ~ 5,
        month == "６月" ~ 6,
        month == "７月" ~ 7,
        month == "８月" ~ 8,
        month == "９月" ~ 9,
        month == "10月" ~ 10,
        month == "11月" ~ 11,
        month == "12月" ~ 12
      )
    ) %>%
    pivot_longer(
      cols = "全国":"沖縄県",
      values_to = "hire",
      names_to = "prefecture"
    ) %>%
    mutate(
      year_month = make_date(year = year, month = month, day = 1),
      data_type = sheet_names[i]) %>%
    select(prefecture, 
           data_type,
           year_month,
           year,
           month,
           hire)
  
  
  list_hire[[i]] <- data_i
}


## listの中に入っているデータをleft_joinでつなげる
data_long <- list_hire[[1]]

for(i in setdiff(1:length(sheet_names), 1)){
  data_long <- data_long %>%
    bind_rows(list_hire[[i]])
}

## パートを含むかどうかで分ける
data_long <- data_long %>%
  mutate(type = case_when(
    str_detect(data_type, pattern = "パート含む一般") ~ "both",
    str_detect(data_type, pattern = "パート除く一般") ~ "full-time",
    str_detect(data_type, pattern = "一般パート") ~ "part-time",
    str_detect(data_type, pattern = "パート含む常用") ~ "both",
    str_detect(data_type, pattern = "パート除く常用") ~ "full-time",
    str_detect(data_type, pattern = "常用的パート") ~ "part-time",
  ),
  contract_type = case_when(
    str_detect(data_type, pattern = "パート含む一般") ~ "both",
    str_detect(data_type, pattern = "パート除く一般") ~ "both",
    str_detect(data_type, pattern = "一般パート") ~ "both",
    str_detect(data_type, pattern = "パート含む常用") ~ "long",
    str_detect(data_type, pattern = "パート除く常用") ~ "long",
    str_detect(data_type, pattern = "常用的パート") ~ "long",
  )
  )

hire <- data_long %>%
  select(-data_type) 
# merge
hello_work_data_monthly_prefecture <- vacancy_keep %>%
  left_join(vacancy_new, by = c("prefecture", "year_month", "year", "month", "type", "contract_type")) %>%
  left_join(unemployed_keep, by = c("prefecture", "year_month", "year", "month", "type", "contract_type")) %>%
  left_join(unemployed_new, by = c("prefecture", "year_month", "year", "month", "type", "contract_type")) %>%
  left_join(hire, by = c("prefecture", "year_month", "year", "month", "type", "contract_type"))

## 就業地ごとのvacancyをjoin、その他の値については同値よりjoinしない
prefecture_data_shugyochi <- prefecture_data %>%
  select(type, 
         contract_type,
         prefecture,
         year_month, 
         year, 
         month,
         vacancy_new_shugyo,
         vacancy_keep_shugyo
  )

hello_work_data_monthly_prefecture <- hello_work_data_monthly_prefecture %>%
  left_join(prefecture_data_shugyochi, by = c("type", "contract_type", 
                                              "prefecture", "year_month", 
                                              "year", "month")) 

## prefectureと日付以外のすべての変数を数値化
## 数値化する変数のベクトル
var_vec <- names(hello_work_data_monthly_prefecture)
var_vec <- setdiff(var_vec, c("year_month", "prefecture", "type", "contract_type"))

for(i in var_vec){
  eval(parse(text = paste0(
    "hello_work_data_monthly_prefecture <- hello_work_data_monthly_prefecture %>% mutate(", i," = as.numeric(", i,"))"
  )
  )
  )
}

## 変数をnumeric化
vec_name <- hello_work_data_monthly_prefecture %>% names()
vec_name <- setdiff(vec_name, c("prefecture", "year_month", 
                                "contract_type", "type"))

for(i in vec_name){
  eval(
    parse(text = paste0(
      "hello_work_data_monthly_prefecture <- hello_work_data_monthly_prefecture %>% mutate(", i," = as.numeric(", i, "))"
    )
    )
  )
}



## hello_work_data_monthly_prefectureの結果を確認
hello_work_data_monthly_prefecture_hokkaido <- hello_work_data_monthly_prefecture %>%
  filter(
    prefecture == "北海道",
    type == "both",
    contract_type == "both"
  )

# 男女別
sheet_names <- excel_sheets("input/第17表.xlsx")

list_gender <- list()

## 求人倍率系のデータは必要ないので、それはデータ整形しない。
## データの形も違う

for(i in 1:length(sheet_names)){
  print(i)
  ## data reading
  data_i <- read_xlsx("input/第17表.xlsx",
                      sheet = i,
                      skip = 1
  ) %>%
    rename(month = ...3) %>%
    drop_na(month) %>% ## 年次の月平均データなどを落としている
    mutate( ## yearとmonthを整備
      year = case_when(
        西暦 == "2013年" ~ 2013,
        西暦 == "2014年" ~ 2014,
        西暦 == "2015年" ~ 2015,
        西暦 == "2016年" ~ 2016,
        西暦 == "2017年" ~ 2017,
        西暦 == "2018年" ~ 2018,
        西暦 == "2019年" ~ 2019,
        西暦 == "2020年" ~ 2020,
        西暦 == "2021年" ~ 2021,
        西暦 == "2022年" ~ 2022,
        西暦 == "2023年" ~ 2023,
        西暦 == "2024年" ~ 2024
      ),
      month = case_when(
        month == "１月" ~ 1,
        month == "２月" ~ 2,
        month == "３月" ~ 3,
        month == "４月" ~ 4,
        month == "５月" ~ 5,
        month == "６月" ~ 6,
        month == "７月" ~ 7,
        month == "８月" ~ 8,
        month == "９月" ~ 9,
        month == "10月" ~ 10,
        month == "11月" ~ 11,
        month == "12月" ~ 12
      )
    ) %>%
    pivot_longer(
      cols = "全国":"沖縄県",
      values_to = "number",
      names_to = "prefecture"
    ) %>%
    mutate(
      year_month = make_date(year = year, month = month, day = 1),
      data_type = sheet_names[i]
    ) %>%
    select(prefecture, 
           data_type,
           year_month,
           year,
           month,
           number)
  
  
  list_gender[[i]] <- data_i
}


## listの中に入っているデータをleft_joinでつなげる
data_long <- list_gender[[1]]

for(i in setdiff(1:length(sheet_names), 1)){
  data_long <- data_long %>%
    bind_rows(list_gender[[i]])
}

## 男女で分ける
data_long_wide <- data_long %>%
  mutate(outcome = case_when(
    str_detect(data_type, pattern = "新規求職申込件数") ~ "unemployed_new",
    str_detect(data_type, pattern = "有効求職者数") ~ "unemployed_keep",
    str_detect(data_type, pattern = "就職件数") ~ "hire"
  ),
  gender_type = case_when(
    str_detect(data_type, pattern = "男女計") ~ "both",
    str_detect(data_type, pattern = "男") ~ "male",
    str_detect(data_type, pattern = "女") ~ "female"
  )
  ) %>%
  select(-data_type) %>%
  pivot_wider(
    values_from = "number",
    names_from = "outcome"
  )



# 職業種類別 ----
sheet_names <- excel_sheets("input/第21表.xlsx")

list_job_type <- list()

## 求人倍率系のデータは必要ないので、それはデータ整形しない。
## データの形も違う
set_diff_vec <- setdiff(1:length(sheet_names), 
                        c(6, 7, 13, 14, 20, 21))

for(i in set_diff_vec){
  print(i)
  ## data reading
  data_i <- read_xlsx("input/第21表.xlsx",
                      sheet = i,
                      skip = 1) %>%
    t() %>%
    as_tibble() 
  
  ## 先にmonth. unit変数を作成
  data_i[1, 2] <- "month"
  data_i[1, 3] <- "unit"
  
  ## jobの種類情報を抜き出し
  job_info <- data_i[1, ]
  
  ## 変数名のrename
  names(data_i) <- job_info
  
  ## 一行目を欠損させる
  data_i <- data_i[-1, ]
  
  ## unitを除去し、ロング化
  data_i <- data_i %>%
    select(-unit,
           -`（注１）平成23年改定「厚生労働省職業分類」に基づく区分。`,                    
           -`（注２）介護関係職種は、「福祉施設指導専門員」、「その他の社会福祉の専門的職業」、「家政婦（夫）、家事手伝」、「介護サービスの職業」の合計。`,
           -`（注３）年・年度別は月平均。` ) %>%
    pivot_longer(cols = "職業計":"介護関係職種（注２）",
                 values_to = sheet_names[i],
                 names_to = "job_kinds")
  
  ## month dummyの欠損している行を除去（年次平均データだと思われる）
  data_i <- data_i %>% drop_na(month)
  
  ## data.frame化
  data_i <- data_i %>% tibble()
  
  list_job_type[[i]] <- data_i
}


## listの中に入っているデータをleft_joinでつなげる
data_long <- list_job_type[[1]]

for(i in setdiff(set_diff_vec, 1)){
  data_long <- data_long %>%
    left_join(list_job_type[[i]], by = c("和暦", "month", "job_kinds"))
}


## マイナーな作業を以下で行う
## 和暦と月をcase_whenで変える。
data_long <- data_long %>%
  mutate(
    year = case_when(
      和暦 == "平成24年" ~ 2012,
      和暦 == "平成25年" ~ 2013,
      和暦 == "平成26年" ~ 2014,
      和暦 == "平成27年" ~ 2015,
      和暦 == "平成28年" ~ 2016,
      和暦 == "平成29年" ~ 2017,
      和暦 == "平成30年" ~ 2018,
      和暦 == "平成31年" ~ 2019,
      和暦 == "令和元年" ~ 2019,
      和暦 == "令和2年" ~ 2020,
      和暦 == "令和3年" ~ 2021,
      和暦 == "令和4年" ~ 2022,
      和暦 == "令和5年" ~ 2023
    ),
    month = case_when(
      month == "1月" ~ 1,
      month == "2月" ~ 2,
      month == "3月" ~ 3,
      month == "4月" ~ 4,
      month == "5月" ~ 5,
      month == "6月" ~ 6,
      month == "7月" ~ 7,
      month == "8月" ~ 8,
      month == "9月" ~ 9,
      month == "10月" ~ 10,
      month == "11月" ~ 11,
      month == "12月" ~ 12
    ),
    year_month = make_date(year = year, month = as.numeric(month), day = 1)
  ) %>%
  select(-和暦)

## パートを含むか含まないかで分ける
data_long_panel <- data_long %>%
  pivot_longer(cols = "第２１表ー１　新規求人（パート含む常用）" :"第２１表ー１９　就職件数（常用的パート）" ,
               names_to = "variable_name",
               values_to = "number")

## 職業形態別変数を作成
data_long_panel <- data_long_panel %>%
  mutate(
    type = case_when(
      str_detect(variable_name, pattern = "パート含む常用") ~ "both",
      str_detect(variable_name, pattern = "パート除く常用") ~ "full-time",
      str_detect(variable_name, pattern = "常用的パート") ~ "part-time"
    ))

## 新規求人・有効求人・新規求職・有効求職・就職者件数変数の作成（かつ、wide化）
data_long_panel <- data_long_panel %>%
  mutate(
    variable_type = case_when(
      str_detect(variable_name, pattern = "新規求人") ~ "vacancy_new",
      str_detect(variable_name, pattern = "有効求人") ~ "vacancy_keep",
      str_detect(variable_name, pattern = "新規求職") ~ "unemployed_new",
      str_detect(variable_name, pattern = "有効求職") ~ "unemployed_keep",
      str_detect(variable_name, pattern = "就職件数") ~ "hire"
    ))

## 不必要な変数を除外し、各vacancy, unemployed, hireの変数を列にする
hello_work_data_monthly_job_category <- data_long_panel %>%
  select(-variable_name) %>%
  pivot_wider(names_from = "variable_type",
              values_from = "number") %>%
  select(
    job_kinds,
    type,
    year_month,
    year,
    month,
    vacancy_new,
    vacancy_keep,
    unemployed_new,
    unemployed_keep,
    hire
  )

## 必要な変数を数値化
var_vec <- c("year",
             "month",
             "vacancy_new",
             "vacancy_keep",
             "unemployed_new",
             "unemployed_keep",
             "hire"
)

for(i in var_vec){
  eval(
    parse(text = paste0(
      "hello_work_data_monthly_job_category <- hello_work_data_monthly_job_category %>% mutate(", i," = as.numeric(", i,"))"
    )
    )
  )
}



# save ----
write_rds(
  hello_work_data_monthly_prefecture,
  file = "cleaned/hello_work_data_monthly_prefecture.rds"
)
write_rds(
  hello_work_data_monthly_job_category,
  file = "cleaned/hello_work_data_monthly_job_category.rds"
)
write_rds(
  data_long_wide,
  file = "cleaned/unemployed_gender.rds"
  )

write_rds(
  prefecture_data,
  file = "cleaned/vacancy_unemployed_hire_prefectrue_non_job_type.rds"
  )

prefecture_data %>%
  filter(
    prefecture == "全国",
    year >= 2012, 
    year <= 2016,
    type == "both",
    contract_type == "both"
  ) %>%
  ggplot(aes(x = year_month,
             #                color = prefecture
  )) +
  #     geom_line(aes(y = log(vacancy_keep))) +
  geom_smooth(aes(y = log(vacancy_keep)), se = FALSE) +
  #     geom_line(aes(y = log(unemployed_keep))) +
  geom_smooth(aes(y = log(unemployed_keep)), se = FALSE)
graph <- hello_work_data_monthly_prefecture %>%
  #        mutate(vacancy_keep = as.numeric(vacancy_keep)) %>%
  filter(
    year <= 2019,
    prefecture == "全国",
    #        prefecture == "北海道",
    contract_type == "both",
    type == "both"
  ) %>%
  ggplot(aes(
    x = year_month,
    #             color = prefecture
  )) +
  geom_line(aes(y = log(vacancy_keep)), color = "red") +
  geom_line(aes(y = log(unemployed_keep)), color = "blue") 
#        theme(legend.position = "none")

graph %>%
  ggplotly()