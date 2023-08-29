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
flu_baseline_all <- readr::read_rds("data/flu_baseline_all.rds")
flu_mean_21_22 <- readr::read_rds("data/flu_mean-ensemble_21-22.rds")
flu_mean_22_23 <- readr::read_rds("data/flu_mean-ensemble_22-23.rds")
flu_median_21_22 <- readr::read_rds("data/flu_median-ensemble_21-22.rds")
flu_median_22_23 <- readr::read_rds("data/flu_median-ensemble_22-23.rds")
flu_linear_pool_21_22 <- readr::read_rds("data/flu_linear_pool-ensemble_21-22.rds")
flu_linear_pool_22_23 <- readr::read_rds("data/flu_linear_pool-ensemble_22-23.rds")


# all_flu_df <- sort(paste("flu", rep(c("mean", "median", "linear_pool", "linear_pool_lognorm"), 2), c(rep("21_22", 4), rep("22_23", 4)), sep="_"))
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
library(covidHubUtils)
hub_locations_flusight <- tibble(hub_locations_flusight)
flu_forecasts_wide <- flu_forecasts_ensembles |>
#  dplyr::mutate(temporal_resolution = "wk") |>
  dplyr::left_join(hub_locations_flusight, by = c("location" = "fips")) |>
  dplyr::mutate(full_location_name=location_name)
View(flu_forecasts_wide)

flu_test <- filter(flu_forecasts_wide, forecast_date < as.Date("2022-07-08"))

flu_forecasts_wide_22 <- filter(flu_forecasts_wide, forecast_date < as.Date("2022-07-08"))
flu_forecasts_wide_23 <- filter(flu_forecasts_wide, forecast_date > as.Date("2022-07-08"))

# Plot Forecasts
p <- covidHubUtils::plot_forecasts(
            forecast_data=flu_forecasts_wide,
            hub = "FluSight",
            models = "02",
            models = c("mean-ensemble", "median-ensemble", "lp-normal"),
            truth_data = flu_truth_all,
#            target_variable = "inc hosp",
            intervals = c(0.5, 0.95),
            truth_source = "HealthData",
            use_median_as_point = TRUE,
            facet = location ~.,
            horizon = 4,
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



# Score Forecasts
# forecast columns: model, forecast_date, location, horizon, temporal_resolution,
#                   target_variable, target_end_date, type, quantile, value
# truth_columns: model, target_variable, target and date, location, value
  
flu_baseline_scores <- flu_baseline_all |> 
 covidHubUtils::score_forecasts(flu_truth_all, return_format="wide", use_median_as_point=TRUE)

flu_scores_ensembles <- flu_forecasts_ensembles |> 
   covidHubUtils::score_forecasts(flu_truth_all, return_format="wide", use_median_as_point=TRUE)

readr::write_rds(flu_scores_ensembles, "data/flu_scores_ensembles.rds", "xz", compression = 9L)
flu_scores_ensembles <- readr::read_rds("data/flu_scores_ensembles.rds")
flu_scores_all <- rbind(flu_scores_ensembles, flu_scores_baseline)
