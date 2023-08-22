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
flu_mean_21_22 <- readr::read_rds("data/flu_mean-ensemble_21-22.rds", "xz", compression = 9L)
flu_mean_22_23 <- readr::read_rds("data/flu_mean-ensemble_22-23.rds", "xz", compression = 9L)
flu_median_21_22 <- readr::read_rds("data/flu_median-ensemble_21-22.rds", "xz", compression = 9L)
flu_median_22_23 <- readr::read_rds("data/flu_median-ensemble_22-23.rds", "xz", compression = 9L)
flu_linear_pool_21_22 <- readr::read_rds("data/flu_linear_pool-ensemble_21-22.rds", "xz", compression = 9L)
flu_linear_pool_22_23 <- readr::read_rds("data/flu_linear_pool-ensemble_22-23.rds", "xz", compression = 9L)


flu_21_22_truth <- covidHubUtils::load_truth(
    truth_end_date="2023-05-15",
    data_location="covidData",
    hub = "FluSight"
)

origin_dates <-  "2023-05-15"
# Query truth
truth_data <- zoltar_connection |> 
  do_zoltar_query(project_url, query_type ="truth", 
#                  units = c("loc1", "loc2"), 
#                  targets = "week ahead incident hospitalizations",
                  timezeros = origin_dates, 
                  types = "quantile") |>
  dplyr::mutate(model_id = "flu-truth", .before = 1) |>
  tidyr::separate(target, sep=2, convert=TRUE, into=c("horizon", "target")) 
# need to match forecasts format
# also must have: model, forecast_date, location, horizon, temporal_resolution, target_variable, horizon, target_end_date
# to use score_forecasts() function
                  
# Score Forecasts
flu_mean_21_22_scores <- flu_mean_21_22 |> 
  covidHubUtils::score_forecasts(return_format="wide", truth=flu_21_22_truth)