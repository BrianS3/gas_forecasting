# ---------------------------------------------------------------------------
# Loads the U.S. national average gas price and adds political context.
# Sourced by gas_EDA.Rmd, gas_forecasting_results.Rmd and gas_forecasting_process.R
#
# Creates:
#   gas_weekly        weekly national average price + who was in control
#   gas_monthly       monthly average price (complete months only)
#   presidencies      one row per presidential term (for shading charts)
#   control_periods   political_control with an end date added
# ---------------------------------------------------------------------------

library(DBI)
library(RSQLite)
library(tidyverse)
library(lubridate)
library(here)

# Other projects (like the blog) can set gas_dir to this project's folder first.
if (!exists("gas_dir")) gas_dir <- here()

source(file.path(gas_dir, "R", "context_tables.R"))

# --- 1. Weekly national average price --------------------------------------
con <- dbConnect(RSQLite::SQLite(), file.path(gas_dir, "database", "gas_data.db"))

gas_weekly <- dbGetQuery(con, "
  select period, price
  from gas_prices
  where padd = 'U.S.'
  order by period
")

dbDisconnect(con)

gas_weekly <- gas_weekly %>%
  as_tibble() %>%
  transmute(date = as.Date(period), price = round(price, 3))

# --- 2. Who was in control each week ---------------------------------------
control_periods <- political_control %>%
  arrange(start) %>%
  mutate(end = lead(start, default = Sys.Date() + 1),
         control = case_when(
           pres_party == house & house == senate ~ paste("Unified", pres_party),
           TRUE                                  ~ "Divided"
         ))

# match each week to the most recent control period that started on/before it
gas_weekly <- gas_weekly %>%
  left_join(control_periods %>% select(-end),
            by = join_by(closest(date >= start))) %>%
  select(-start)

# --- 3. Monthly averages (complete months only) ----------------------------
first_day_of_current_month <- floor_date(Sys.Date(), "month")

gas_monthly <- gas_weekly %>%
  mutate(month = floor_date(date, "month")) %>%
  filter(month < first_day_of_current_month) %>%
  group_by(month) %>%
  summarise(price = round(mean(price), 3), .groups = "drop")

# --- 4. One row per presidential term (consecutive weeks, same president) --
presidencies <- control_periods %>%
  mutate(term_id = consecutive_id(president)) %>%
  group_by(term_id, president, pres_party) %>%
  summarise(start = min(start), end = max(end), .groups = "drop") %>%
  mutate(label = paste0(president, " (", year(start), "-",
                        ifelse(end > Sys.Date(), "now", year(end)), ")"))

# --- 5. Colors --------------------------------------------------------------
party_colors <- c("D" = "#2B6CB0", "R" = "#C53030")
