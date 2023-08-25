library(tidyverse)
library(zoltr)
library(dplyr)
library(lubridate)
library(readr)
library(tidyr)
library(stringr)
library(hubUtils)
library(hubEnsembles)

# Generate ensembles
#flu_locations <- forecast_data$unit |> unique() |> sort()
flu_dates_21_22 <- as.Date("2022-01-24") + weeks(0:21)
flu_dates_22_23 <- as.Date("2022-10-17") + weeks(0:30)
all_flu_dates <- c(flu_dates_21_22, flu_dates_22_23)

# Read in data
flu_truth_all <- readr::read_rds("data/flu_truth_all.rds")
flu_mean_21_22 <- readr::read_rds("data/flu_mean-ensemble_21-22.rds")
flu_mean_22_23 <- readr::read_rds("data/flu_mean-ensemble_22-23.rds")
flu_median_21_22 <- readr::read_rds("data/flu_median-ensemble_21-22.rds")
flu_median_22_23 <- readr::read_rds("data/flu_median-ensemble_22-23.rds")
flu_linear_pool_21_22 <- readr::read_rds("data/flu_linear_pool-ensemble_21-22.rds")
flu_linear_pool_22_23 <- readr::read_rds("data/flu_linear_pool-ensemble_22-23.rds")


# all_flu_df <- sort(paste("flu", rep(c("mean", "median", "linear_pool", "linear_pool_lognorm"), 2), c(rep("21_22", 4), rep("22_23", 4)), sep="_"))
flu_files <- list.files(path="data", pattern="ensemble", full.names=TRUE)
flu_forecasts_all <- purrr::map_dfr(flu_files, .f=read_rds) |>
    dplyr::select(model, forecast_date, location, horizon, temporal_resolution, 
                  target_variable, target_end_date, type, quantile, value)

flu_truth_21_22 <- flu_truth_all |>
  dplyr::filter(target_end_date < "2022-08-01")
flu_truth_22_23 <- flu_truth_all |>
  dplyr::filter(target_end_date > "2022-08-01")


# Score Forecasts
# forecast columns: model, forecast_date, location, horizon, temporal_resolution,
#                   target_variable, target_end_date, type, quantile, value
# truth_columns: model, target_variable, target and date, location, value
flu_mean_21_22_scores <- flu_mean_21_22 |> 
  covidHubUtils::score_forecasts(flu_truth_21_22, return_format="wide", use_median_as_point=TRUE)
  
flu_scores_all <- flu_forecasts_all |> 
  covidHubUtils::score_forecasts(flu_truth_all, return_format="wide", use_median_as_point=TRUE)

readr::write_rds(flu_scores_all, "data/flu_scores_all.rds", "xz", compression = 9L)
