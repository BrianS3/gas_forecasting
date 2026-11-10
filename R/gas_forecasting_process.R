# ---------------------------------------------------------------------------
# 2-month national gas price projection
#
# 1. Build a monthly series of the U.S. national average price.
# 2. Backtest 3 simple models: for each of the last 24 months, pretend it is
#    that month, project the next 2 months, and compare the 2-month average
#    to what actually happened.
# 3. Keep the model with the smallest average miss (MAE, in dollars).
# 4. Project the next 2 months with that model and save results to the DB.
#
# Writes these tables to database/gas_data.db (replaced each run):
#   forecast_monthly    the 2 projected months
#   forecast_two_month  the single 2-month average projection
#   forecast_backtest   every backtest projection vs. actual
#   forecast_accuracy   average miss per model
# ---------------------------------------------------------------------------

library(here)
source(here("R", "gas_data_prep.R"))
library(tsibble)
library(fable)

set.seed(2026)

backtest_months <- 24   # how many past months to test against
horizon         <- 2    # months ahead to project

# --- 1. Monthly series ------------------------------------------------------
gas_ts <- gas_monthly %>%
  mutate(month = yearmonth(month)) %>%
  as_tsibble(index = month)

# The 3 candidate models:
#   naive : next 2 months = last month (hard to beat for gas prices)
#   ets   : exponential smoothing, picks trend/seasonality automatically
#   arima : ARIMA on log price, picks its own settings automatically
fit_models <- function(data) {
  data %>% model(
    naive = NAIVE(price),
    ets   = ETS(price),
    arima = ARIMA(log(price))
  )
}

# --- 2. Backtest ------------------------------------------------------------
# stretch_tsibble makes one copy of the data per test month (.id),
# each copy one month longer than the last.
first_cutoff <- nrow(gas_ts) - backtest_months - horizon + 1

backtest_forecasts <- gas_ts %>%
  stretch_tsibble(.init = first_cutoff, .step = 1) %>%
  fit_models() %>%
  forecast(h = horizon)

actuals <- gas_ts %>% as_tibble() %>% rename(actual = price)

backtest <- backtest_forecasts %>%
  as_tibble() %>%
  select(.id, model = .model, month, projected = .mean) %>%
  left_join(actuals, by = "month") %>%
  group_by(.id, model) %>%
  filter(!any(is.na(actual))) %>%            # skip windows we can't score yet
  summarise(start_month = as.Date(min(month)),
            projected   = mean(projected),
            actual      = mean(actual),
            .groups = "drop") %>%
  mutate(error = projected - actual) %>%
  select(-.id)

accuracy <- backtest %>%
  group_by(model) %>%
  summarise(avg_miss_dollars = mean(abs(error)),     # MAE
            avg_bias_dollars = mean(error),          # + means projections ran high
            windows_tested   = n(),
            .groups = "drop") %>%
  arrange(avg_miss_dollars)

best_model <- accuracy$model[1]
print(accuracy)
message("Best model: ", best_model)

# --- 3. Final projection ----------------------------------------------------
final_fit <- gas_ts %>% fit_models() %>% select(all_of(best_model))

monthly_projection <- final_fit %>%
  forecast(h = horizon) %>%
  hilo(80) %>%
  unpack_hilo(`80%`) %>%
  as_tibble() %>%
  transmute(month     = as.Date(month),
            projected = .mean,
            lower_80  = `80%_lower`,
            upper_80  = `80%_upper`,
            model     = best_model)

# The range for the 2-month average comes from simulating 2,000 possible
# 2-month paths and averaging each one.
two_month_projection <- final_fit %>%
  generate(h = horizon, times = 2000) %>%
  as_tibble() %>%
  group_by(.rep) %>%
  summarise(avg = mean(.sim)) %>%
  summarise(projected = mean(avg),
            lower_80  = quantile(avg, 0.10),
            upper_80  = quantile(avg, 0.90)) %>%
  mutate(projected   = mean(monthly_projection$projected),  # match the monthly table
         start_month = min(monthly_projection$month),
         end_month   = max(monthly_projection$month),
         last_actual_month = max(gas_monthly$month),
         model       = best_model,
         avg_miss_dollars = accuracy$avg_miss_dollars[1],
         run_date    = Sys.Date())

print(monthly_projection)
print(two_month_projection)

# --- 4. Save ----------------------------------------------------------------
dates_to_text <- function(df) mutate(df, across(where(is.Date), as.character))

con <- dbConnect(RSQLite::SQLite(), here("database", "gas_data.db"))
dbWriteTable(con, "forecast_monthly",   dates_to_text(monthly_projection),   overwrite = TRUE)
dbWriteTable(con, "forecast_two_month", dates_to_text(two_month_projection), overwrite = TRUE)
dbWriteTable(con, "forecast_backtest",  dates_to_text(backtest),             overwrite = TRUE)
dbWriteTable(con, "forecast_accuracy",  accuracy,                            overwrite = TRUE)
dbDisconnect(con)
