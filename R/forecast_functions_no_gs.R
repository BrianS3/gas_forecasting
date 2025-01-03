library(fable)
library(fabletools)
library(feasts)
# library(ggplot2)
library(glue)
# library(janitor)
library(lubridate)
# library(magrittr)
# library(progress)
# library(stringr)
# library(tidymodels)
# library(tidyr)
library(tidyverse)
library(tsibble)
# library(tsibbledata)
library(zoo)

#' Creates global parameters for other functions, these are
#' the time series start year, time series end year, test period start, test period end
#' 
#' @param fcst_start
#' @param fcst_end
#' @param fcst_test_start
#' @param fcst_test_end
#' 
#' @return None, creates global objects

create_global_start_end <- function(fcst_start, fcst_end, fcst_test_start, fcst_test_end) {
  global_fcst_start <<- as.Date(fcst_start)
  global_fcst_end <<- as.Date(fcst_end)
  global_fcst_test_start <<- as.Date(fcst_test_start)
  global_fcst_test_end <<- as.Date(fcst_test_end)
}

#' Create Interval in Months
#'
#' Calculates the interval in months based on the provided dates.
#'
#' @description
#' This function calculates the number of months between two dates and assigns the results to global variables
#' \code{train_interval_months} and \code{forecast_interval_months}.
#'
#' @details
#' The function uses the \code{as.yearmon} function from the \code{zoo} package to convert the dates into year-month format.
#' It then calculates the difference between the dates in months using \code{as.numeric}, and assigns the results to
#' \code{train_interval_months} and \code{forecast_interval_months} as floor-rounded values.
#' The \code{train_interval_months} is calculated as the number of months between \code{global_fcst_test_start} and \code{global_fcst_test_end},
#' while \code{forecast_interval_months} is calculated as the number of months between \code{global_fcst_test_end} and the end of the next year.
create_interval_months <- function(){
  train_interval_months <<- floor((as.numeric(as.yearmon(global_fcst_test_end)-as.yearmon(global_fcst_test_start)) * 12)+1)
  forecast_interval_months <<- 6
}

#' Creates test/train split on tsibble object
#' 
#' @param input_tsibble tsibble object to be split into test/train
#'
#' @return list containing the training set, test set, and forecast training set
#' 
test_train_split <- function(input_tsibble) {
  train_start <- format(global_fcst_start, "%Y %b")
  train_end <- format(global_fcst_test_start %m-% months(1), "%Y %b")
  test_start <- format(global_fcst_test_start, "%Y %b")
  test_end <- format(global_fcst_test_end, "%Y %b")
  train <- input_tsibble %>% filter_index(train_start ~ train_end)
  test <- input_tsibble %>% filter_index(test_start ~ test_end)
  fcst_train <- input_tsibble %>% filter_index(train_start ~ test_end)
  
  return(list(train = train, test = test, fcst_train = fcst_train))
}


#'
#' @param train training dataset, used to fit the models
#' @param test testing dataset, used to evaluate the models
#' 
#' @return A data list of accuracy results and forecast

create_forecast <- function(train, test) {
  model <- train %>%  model(
    mean = MEAN(value),
    mean_log = MEAN(log(value)),
    snaive = SNAIVE(value),
    snaive_log = SNAIVE(log(value)),
    ets = ETS(value),
    ets_log = ETS(log(value)),
    arima = ARIMA(value),
    arima_log = ARIMA(log(value)),
    rw = RW(value),
    rw_log = RW(log(value))
  )
  
  fcsts <- model %>% forecast(h = glue::glue("{train_interval_months} months"))
  accuracy_result <- accuracy(fcsts, test) %>% select(.model, MAE, MAPE)
  
  fcsts_out <- fcsts %>%
    as_tibble() %>% 
    filter(
      .model == !!pull(accuracy_result[accuracy_result$MAPE==min(accuracy_result$MAPE),".model"])
    ) %>% 
    transmute(date = as.Date(year_month), value = .mean)

  
  
  return(list(results = accuracy_results, forecast=fcsts_out))
}


