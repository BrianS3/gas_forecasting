library(here)
source("R/forecast_functions.R")

retrain <- FALSE

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

if (retrain) {

test_train <- test_train_split(gas_national_average_tsibble)

test <- test_train[['test']]
train <- test_train[['train']]

gr_results <- grid_search(forecast_label = "national_average", train = train, test = test)

saveRDS(gr_results, "R/gr_results.RDS")

}

gr_results <- readRDS("R/gr_results.RDS")

best_model <- get_best_tuning_results(gr_results)



train_results <- create_forecasts(best_model = best_model, fcst_training_data = train)
fcst_results <- create_forecasts(best_model = best_model, fcst_training_data = gas_national_average_tsibble)

train_fcst <- train_results[['fcst']]
forecast <- fcst_results[['fcst']]
model <- fcst_results[['model']]

saveRDS(model, "forecast_model.RDS")

final_forecast <- rbind(forecast %>% mutate(forecast=1), train_fcst %>% mutate(forecast=2))

dbExecute(con, "DELETE FROM 'forecast_results'")
dbWriteTable(con, "forecast_results", final_forecast %>% mutate(date=as.character(date)), append = TRUE, row.names = FALSE)




