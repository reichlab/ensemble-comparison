library(tidyverse)
library(zoltr)
library(dplyr)
library(lubridate)
library(readr)
library(tidyr)
library(stringr)
library(hubUtils)
library(hubEnsembles)
library(covidHubUtils)

library(patchwork)
source("R/evaluation_functions.R")

# Generate ensembles
#flu_locations <- forecast_data$unit |> unique() |> sort()
flu_dates_21_22 <- as.Date("2022-01-24") + weeks(0:21)
flu_dates_22_23 <- as.Date("2022-10-17") + weeks(0:30)
all_flu_dates <- c(flu_dates_21_22, flu_dates_22_23)

# Read in data
flu_truth_all <- readr::read_rds("data/flu_truth_all.rds")
flu_baseline_all <- readr::read_rds("data/flu_baseline_all.rds")
flu_mean_21_22 <- readr::read_rds("data/flu_mean-ensemble_21-22.rds")
flu_mean_22_23 <- readr::read_rds("data/flu_mean-ensemble_22-23.rds")
flu_median_21_22 <- readr::read_rds("data/flu_median-ensemble_21-22.rds")
flu_median_22_23 <- readr::read_rds("data/flu_median-ensemble_22-23.rds")
flu_linear_pool_21_22 <- readr::read_rds("data/flu_linear_pool-ensemble_21-22.rds")
flu_linear_pool_22_23 <- readr::read_rds("data/flu_linear_pool-ensemble_22-23.rds")


flu_files <- list.files(path="data", pattern="ensemble_", full.names=TRUE)
flu_forecasts_ensembles <- purrr::map_dfr(flu_files, .f=read_rds) |>
    dplyr::select(model, forecast_date, location, horizon, temporal_resolution, 
                  target_variable, target_end_date, type, quantile, value)

flu_truth_21_22 <- flu_truth_all |>
  dplyr::filter(target_end_date < "2022-08-01")
flu_truth_22_23 <- flu_truth_all |>
  dplyr::filter(target_end_date > "2022-08-01")


# Plot Forecasts
# Plotting columns: model, forecast_date, location, horizon, temporal_resolution,
#                   target_variable, target_end_date, type, quantile, value,
#                   abbreviation, location_name, population, full_location_name
hub_locations_flusight <- tibble(hub_locations_flusight)
flu_forecasts_wide <- flu_forecasts_ensembles |>
  dplyr::left_join(hub_locations_flusight, by = c("location" = "fips")) #|>
#  dplyr::mutate(geo_type = "state", geo_value = stringr::str_to_lower(abbreviation), full_location_name=location_name) |>
#  dplyr::select(1:11, location_name, population, geo_type, geo_value, abbreviation, full_location_name)
#View(flu_forecasts_wide)

flu_forecasts_test <- flu_forecasts_wide |>
  dplyr::filter(quantile == 0.5) |>
  dplyr::mutate(quantile = NA, type = "point") |>
  rbind(flu_forecasts_wide)
  
flu_baseline_wide <- flu_baseline_all |>
  dplyr::left_join(hub_locations_flusight, by = c("location" = "fips")) |>
  dplyr::mutate(geo_type = "state", geo_value = stringr::str_to_lower(abbreviation), full_location_name=location_name) |>
  dplyr::select(1:11, location_name, population, geo_type, geo_value, abbreviation, full_location_name)
  
flu_test <- filter(flu_forecasts_wide, forecast_date < as.Date("2022-07-08"))

flu_forecasts_wide_22 <- filter(flu_forecasts_wide, forecast_date < as.Date("2022-07-08"))
flu_forecasts_wide_23 <- filter(flu_forecasts_wide, forecast_date > as.Date("2022-07-08"))



flu_forecasts_loaded <- covidHubUtils::load_forecasts(
  models = NULL,
  dates = origin_dates,
  date_window_size = 1,
  locations = NULL,
  types = "quantile",
  targets = NULL,
  source = "local_hub_repo",
  hub_repo_path = "../Flusight-forecast-data/",
  local_zoltpy_path,
  zoltar_sqlite_file,
  data_processed_subpath = "data-forecasts/",
  as_of = NULL,
  hub = "FluSight",
  verbose = TRUE
)
warnings()
unique(flu_forecasts_loaded$model)
forecast_data

flu_forecasts_joined <- flu_forecasts_loaded |>
    dplyr::select(location, abbreviation, location_name, population) |>
    dplyr::distinct(location, .keep_all=TRUE) |>
    dplyr::right_join(flu_forecasts_ensembles, by="location") # still doesn't plot nicely

forecast_data_hub |>
    filter(model_id == "UMass-trends_ensemble") |>
    select(-season) |>
    arrange(location) |>
    testthat::expect_equal(flu_forecasts_loaded)

# Plot Forecasts
p <- plot_forecasts(
#            forecast_data=flu_baseline_wide,
            forecast_data=mutate(flu_forecasts_test, horizon=as.character(horizon)),
#            forecast_data=flu_forecasts_joined,
            hub = "FluSight",
            location = "US",
#            models = c("mean-ensemble", "median-ensemble", "lp-normal"),
            truth_data = select(flu_truth_all, model, location, target_end_date, target_variable, value),
#            target_variable = "inc hosp",
            intervals = c(0.5, 0.95),
            truth_source = "HealthData",
#            use_median_as_point = TRUE,
#            forecast_dates = "2023-05-15",
            facet = model ~.,
#            horizon = 4,
#            facet_nrow = 3,
           facet_scales = "free_y",
            fill_by_model = TRUE,
            plot=FALSE)
      p+
      scale_x_date(name=NULL, #limits = c(as.Date("2022-01-01"), as.Date("2022-07-08")), 
#      scale_x_date(name=NULL, limits = c(as.Date("2023-02-01"), as.Date("2023-07-08")), 
        date_breaks = "2 months", date_labels = "%b '%y") +
#      coord_cartesian(ylim = c(700, 4000)) +
#      coord_cartesian(ylim = c(600, 2500)) +
#      coord_cartesian(ylim = c(0, max(flu_truth_all$value) * 1.5)) +
      theme(axis.ticks.length.x = unit(0.5, "cm"),
            axis.text.x = element_text(vjust = 7, hjust = -0.2),
            legend.position = "none") 

