library(fable)
library(fabletools)
library(feasts)
library(ggplot2)
library(glue)
library(janitor)
library(lubridate)
library(magrittr)
library(progress)
library(stringr)
library(tidymodels)
library(tidyr)
library(tidyverse)
library(tsibble) 
library(tsibbledata)
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

#' This function performs a grid search for hyperparameter tuning on a variety of forecasting models. 
#' The purpose is to identify the optimal parameters that yield the lowest error rates (MAE and MAPE).
#' The function calculates and compares Mean Absolute Error (MAE) and Mean Absolute Percentage Error (MAPE) for each set of parameters.
#' 
#' Models used in this function are: mean, yoy, ets, and arima, each with a distinct set of parameters to be tuned.
#' 
#' @param forecast_label label to be attached to the grid search results
#' @param train training dataset, used to fit the models
#' @param test testing dataset, used to evaluate the models
#' 
#' @return A data frame with the grid search results, including MAE and MAPE for each combination of parameters

grid_search <- function(forecast_label, train, test) {
  
  model_tuning <- function(train, test, models, model_params) {
    results <- data.frame()
    model_count <- length(names(models))
    
    for (i in seq(length(names(models)))) {
      model_name <- names(models[i])
      model_formula <- models[[model_name]]
      params <- model_params[[model_name]]
      
      if (length(params) > 0) {
        if (model_name == "mean") {
          for (param_value in params[["window_size"]]) {
            model_formula_updated <- MEAN(value ~ window(size = param_value))
            model <- train %>% model(!!model_name := model_formula_updated)
            fcsts <- model %>% forecast(h = glue::glue("{train_interval_months} months"))
            accuracy_result <- accuracy(fcsts, test) %>% select(.model, MAE, MAPE)
            accuracy_result[["window_size"]] <- param_value
            results <- bind_rows(results, accuracy_result)
          }
        } else if (model_name == "ets") {
          all_combinations <- expand.grid(params)
          for (i in 1:nrow(all_combinations)) {
            model_formula_updated <- ETS(value ~ error(as.character(all_combinations$error[i])) +
                                           trend(as.character(all_combinations$trend[i])) +
                                           season(as.character(all_combinations$seasonal[i])))
            model <- train %>% model(!!model_name := model_formula_updated)
            fcsts <- model %>% forecast(h = glue::glue("{train_interval_months} months"))
            accuracy_result <- accuracy(fcsts, test) %>% select(.model, MAE, MAPE)
            accuracy_result[["error"]] <- all_combinations$error[i]
            accuracy_result[["trend"]] <- all_combinations$trend[i]
            accuracy_result[["seasonal"]] <- all_combinations$seasonal[i]
            
            results <- bind_rows(results, accuracy_result)
          }
        } else if (model_name == "arima") {
          for (p_value in params$p) {
            for (d_value in params$d) {
              for (q_value in params$q) {
                for (log_transform in params$log_transform) {
                  for (K_value in params$K) {
                    if (log_transform) {
                      model_formula_updated <- ARIMA(log(value) ~ pdq(p_value, d_value, q_value) + fourier(K = K_value))
                    } else {
                      model_formula_updated <- ARIMA(value ~ pdq(p_value, d_value, q_value) + fourier(K = K_value))
                    }
                    
                    model <- train %>% model(!!model_name := model_formula_updated)
                    fcsts <- model %>% forecast(h = length(test))
                    
                    accuracy_result <- accuracy(fcsts, test) %>% select(.model, MAE, MAPE)
                    accuracy_result[["P"]] <- p_value
                    accuracy_result[["D"]] <- d_value
                    accuracy_result[["Q"]] <- q_value
                    accuracy_result[["log_transform"]] <- log_transform
                    accuracy_result[["K"]] <- K_value
                    
                    results <- bind_rows(results, accuracy_result)
                  }
                }
              }
            }
          }
        } else if (model_name == "var") {
          for (order in params$order) {
            model_formula_updated <- VAR(value ~ order(order))
            model <- train %>% model(!!model_name := model_formula_updated)
            fcsts <- model %>% forecast(h = length(test))
            accuracy_result <- accuracy(fcsts, test) %>% select(.model, MAE, MAPE)
            accuracy_result[["order"]] <- order
            results <- bind_rows(results, accuracy_result)
          }
        }
        
      } else {
        model <- train %>% model(!!model_name := model_formula)
        fcsts <- model %>% forecast(h = glue::glue("{train_interval_months} months"))
        accuracy_result <- accuracy(fcsts, test) %>% select(.model, MAE, MAPE)
        
        results <- bind_rows(results, accuracy_result)
      }
    }
    return(results)
  }
  
  # Run the grid search
  models <- list(
    mean = MEAN(value ~ window(size = 6)),
    yoy = SNAIVE(value ~ lag("year")),
    ets = ETS(value ~ error("A") + trend("A") + season("A")),
    arima = ARIMA(log(value) ~ pdq(0, 0, 2)),
    var = VAR(value ~ order(1))
  )
  
  model_params <- list(
    mean = list(window_size = 1:12),
    yoy = list(),
    ets = list(
      error = c("A", "M"),
      trend = c("A", "M", "Z"),
      seasonal = c("A", "M", "Z")
    ),
    arima = list(p = 0:2, d = 0:1, q = 0:2, log_transform = c(TRUE, FALSE), K = 1:4),
    var = list(order = 1:4)
  )
  
  grid_search_results <- model_tuning(train, test, models, model_params) %>% 
    mutate(run_date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
           label = forecast_label)
  return(grid_search_results)
}

#' This function retrieves the best tuning results from a previously executed grid search. 
#' It filters the results based on a specified search term and identifies the model with the lowest Mean Absolute Percentage Error (MAPE).
#' 
#' @param search_term string, used to identify run sequence, e.g. iap, or_cases, etc.
#' 
#' @return A data frame of the best tuning results
get_best_tuning_results <- function(gr_results, specific_label_nths = NULL) {
  
  if (nrow(gr_results) == 0) {
    warning("No results found for the given search term.")
    return(NULL)
  }
  
  # If specific_label is provided, filter the results for this label
  if (!is.null(specific_label_nths)) {
    gr_results_specific <- list()
    
    for(label in names(specific_label_nths)){
      nth_best = specific_label_nths[[label]]
      temp_data <- gr_results %>% 
        dplyr::filter(label == !!label) %>%
        dplyr::group_by(label) %>% 
        dplyr::filter(run_date == max(run_date)) %>%
        dplyr::arrange(MAPE)
      if(nth_best < 1 | nth_best > nrow(temp_data)){
        warning(paste("Invalid nth_best value for label", label))
      } else {
        gr_results_specific[[label]] <- temp_data %>% slice(nth_best)
      }
    }
    gr_results_specific <- bind_rows(gr_results_specific)
  }
  
  
  if (any(is.infinite(gr_results$MAPE))) {
    gr_results <- gr_results %>%
      dplyr::mutate(error_metric = ifelse(is.infinite(MAPE), MAE, MAPE))
  } else {
    gr_results <- gr_results %>%
      dplyr::mutate(error_metric = MAPE)
  }
  
  gr_results <- gr_results %>%
    dplyr::filter(!is.na(error_metric))
  
  gr_results <- gr_results %>%
    dplyr::group_by(label) %>% 
    dplyr::filter(run_date == max(run_date)) %>%
    dplyr::ungroup() %>% 
    dplyr::group_by(label, .model) %>%
    dplyr::filter(error_metric == min(error_metric)) %>%
    dplyr::arrange(error_metric) %>%
    dplyr::distinct(label, .keep_all = TRUE)
  
  # After getting the top model for each label, check if it is a 'mean' model 
  # and if so, look for a model within 2% of its error rate to replace it.
  gr_results <- gr_results %>%
    dplyr::group_by(label) %>%
    dplyr::arrange(error_metric) %>%
    dplyr::mutate(replace_mean = (.model == "mean") & (lead(error_metric) <= 1.02 * error_metric)) %>%
    dplyr::filter(!replace_mean | is.na(replace_mean)) %>%
    dplyr::distinct(label, .keep_all = TRUE)
  
  
  if (!is.null(specific_label_nths)) {
    gr_results <- gr_results %>% filter(!label %in% names(specific_label_nths))
    gr_results <- rbind(gr_results, gr_results_specific)
  }
  
  # Return the nth best model
  return(gr_results)
}


#' Creates a forecast for the next n months based
#' 
#' @param best_model df filtered to specific cut from 'get_best_tuning_results'
#' @param fcst_training_data input training data from 'test_train_split'
#' 
#' @return data frame of forecast for next n months

create_forecasts <- function(best_model, fcst_training_data) {
  if (best_model$.model == "mean") {
    model_formula <- MEAN(value ~ window(size = best_model$window_size))
  } else if (best_model$.model == "ets") {
    model_formula <- ETS(value ~ error(as.character(best_model$error)) +
                           trend(as.character(best_model$trend)) +
                           season(as.character(best_model$seasonal)))
  } else if (best_model$.model == "arima") {
    if (best_model$log_transform) {
      if (!is.null(best_model$K) && !is.na(best_model$K) && length(best_model$K) > 0) {
        model_formula <- ARIMA(log(value) ~ pdq(best_model$p, best_model$d, best_model$q) + fourier(K = best_model$K))
      } else {
        model_formula <- ARIMA(log(value) ~ pdq(best_model$p, best_model$d, best_model$q))
      }
    } else {
      if (!is.null(best_model$K) && !is.na(best_model$K) && length(best_model$K) > 0) {
        model_formula <- ARIMA(value ~ pdq(best_model$p, best_model$d, best_model$q) + fourier(K = best_model$K))
      } else {
        model_formula <- ARIMA(value ~ pdq(best_model$p, best_model$d, best_model$q))
      }
    }
  } else if (model_name == "var") {
    model_formula <- VAR(value ~ order(best_model$order))
  } else {
    model_formula <- SNAIVE(value ~ lag("year"))
  }
  
  trained_model <- fcst_training_data %>% 
    model(!!best_model$.model := model_formula)
  
  fcsts <- trained_model %>% 
    forecast(h = glue::glue("{forecast_interval_months} months"))
  
  fcsts_out <- fcsts %>%
    as_tibble() %>% 
    transmute(date = as.Date(year_month), value = .mean)
  
  return(list(fcst = fcsts_out, model = trained_model))
}
