source("R/forecast_functions.R")

create_global_start_end(
  fcst_start = "1993-04-05",
  fcst_end = floor_date(Sys.Date(),unit = "month")-1,
  fcst_test_start = (floor_date(Sys.Date(), unit = "month")-1) - years(1),
  fcst_test_end = floor_date(Sys.Date(),unit = "month")-1
)

create_interval_months()

con <- dbConnect(RSQLite::SQLite(), paste0(here(),"/database/gas_data.db"))
gas_raw <- dbGetQuery(con, "select * from gas_prices")

gas_monthly_average <- gas_raw %>% 
  mutate(year = year(period), month=month(period), period_month=as.Date(paste0(year,"-", month, "-01"))) %>% 
  filter(period_month < first_day_of_current_month) %>% #only show complete months
  group_by(period_month, padd) %>% 
  summarise(monthly_average = round(mean(price),2), .groups = "drop") %>% 
  mutate(
    padd_descrip = case_when(
      padd=="PADD 1" ~"East Coast",
      padd=="PADD 2" ~"Midwest", 
      padd=="PADD 3" ~"Gulf Coast",
      padd=="PADD 4" ~"Rocky Mountain",
      TRUE ~"West Coast"),
  ) %>% select(-padd)

gas_national_average <- gas_raw %>% 
  mutate(year = year(period), month=month(period), period_month=as.Date(paste0(year,"-", month, "-01"))) %>% 
  filter(as.Date(period_month) < first_day_of_current_month) %>% 
  group_by(period_month) %>% 
  summarise(monthly_average = round(mean(price),2), .groups = "drop") %>% 
  mutate(padd_descrip="National (All Regions)")

gas_national_average_tsibble <- gas_national_average %>% 
  mutate(year_month = make_yearmonth(year = year(period_month), month = month(period_month))) %>% 
  as_tsibble(index = "year_month") %>% 
  transmute(year_month, value = monthly_average)

test_train <- test_train_split(gas_national_average_tsibble)

test <- test_train[['test']]
train <- test_train[['train']]
