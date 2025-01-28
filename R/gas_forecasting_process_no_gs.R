library(here)
library(DBI)
library(tidyverse)
source("R/forecast_functions_no_gs.R")

first_day_of_current_month <- as.Date(format(Sys.Date(), "%Y-%m-01"))

global_fcst_start <<- as.Date("1993-04-05")
global_fcst_end <<- as.Date("2024-11-30") #as.Date(floor_date(Sys.Date(),unit = "month")-1)
global_fcst_test_start <<-as.Date((floor_date(Sys.Date(), unit = "month")-1) - years(1))
global_fcst_test_end <<- as.Date("2024-11-30")  #as.Date(floor_date(Sys.Date(),unit = "month")-1)

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
fcst_train <- test_train[['fcst_train']]

testing_forecast <- create_forecast(train, test, train_interval_months)
full_forecast <- create_forecast(train = train, test=tibble(), interval_months = forecast_interval_months)

out_data <- rbind(
  testing_forecast['forecast'],
  full_forecast
) %>% 
  mutate(forecast_version="jan_25")

dbWriteTable(con, "forecast_results", final_forecast %>% mutate(date=as.character(date)), append = TRUE, row.names = FALSE)




