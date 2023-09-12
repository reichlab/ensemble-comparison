library(tidyverse)
library(zoltr)
library(dplyr)
library(lubridate)
library(readr)
library(tidyr)
library(stringr)
library(hubUtils)
library(hubEnsembles)

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


# all_flu_df <- sort(paste("flu", rep(c("mean", "median", "linear_pool", "linear_pool_lognorm"), 2), c(rep("21_22", 4), rep("22_23", 4)), sep="_"))
flu_files <- list.files(path="data", pattern="ensemble_", full.names=TRUE)
flu_forecasts_ensembles <- purrr::map_dfr(flu_files, .f=read_rds) |>
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
  
flu_baseline_scores <- flu_baseline_all |> 
 covidHubUtils::score_forecasts(flu_truth_all, return_format="wide", use_median_as_point=TRUE)

flu_scores_ensembles <- flu_forecasts_ensembles |> 
   covidHubUtils::score_forecasts(flu_truth_all, return_format="wide", use_median_as_point=TRUE)

readr::write_rds(flu_scores_ensembles, "data/flu_scores_ensembles.rds", "xz", compression = 9L)
flu_scores_ensembles <- readr::read_rds("data/flu_scores_ensembles.rds")
flu_scores_all <- rbind(flu_scores_ensembles, flu_scores_baseline)

# EVALUATION
# Overall
flu_overall_us <- flu_scores_all |>
  evaluate_flu_scores(grouping_variables=NULL, baseline_name="Flusight-baseline", us_only=TRUE)
  
flu_overall_states <- flu_scores_all |>
  evaluate_flu_scores(grouping_variables=NULL, baseline_name="Flusight-baseline", us_only=FALSE)

# Season
flu_season_us <- flu_scores_all |>
  evaluate_flu_scores(grouping_variables="season", baseline_name="Flusight-baseline", us_only=TRUE)

flu_season_states <- flu_scores_all |>
  evaluate_flu_scores(grouping_variables="season", baseline_name="Flusight-baseline", us_only=FALSE)

# Horizon
flu_horizon_us <- flu_scores_all |>
  evaluate_flu_scores(grouping_variables="horizon", baseline_name="Flusight-baseline", us_only=TRUE)

flu_horizon_states <- flu_scores_all |>
  evaluate_flu_scores(grouping_variables="horizon", baseline_name="Flusight-baseline", us_only=FALSE)

model_names <- c("Flusight-baseline", "mean-ensemble", "median-ensemble", "lp-normal")
model_colors <- c("black", "red", "yellow", "green")

wis_plot_us <- plot_evaluated_scores(flu_horizon_us, model_names, model_colors, main="US")
wis_plot_states <- plot_evaluated_scores(flu_horizon_states, model_names, model_colors, main="States")

wis_plot_us + wis_plot_states +
  plot_layout(ncol = 2, guides='collect') &
  theme(legend.position='bottom')

# Location
flu_location <- flu_scores_all |>
  evaluate_flu_scores(grouping_variables="location", baseline_name="Flusight-baseline")

model_levels <- pull(flu_overall_states, model)
plot_wis_loc(flu_scores_all, flu_truth_all, model_levels, baseline_name = "Flusight-baseline")

# Season and Horizon
flu_season_horizon_us <- flu_scores_all |>
  evaluate_flu_scores(grouping_variables=c("horizon", "season"), baseline_name="Flusight-baseline", us_only=TRUE)

flu_season_horizon_states <- flu_scores_all |>
  evaluate_flu_scores(grouping_variables=c("horizon", "season"), baseline_name="Flusight-baseline", us_only=FALSE)

wis_plot_us_2122 <- flu_season_horizon_us |>
  dplyr::filter(season == "2021-2022") |>
  plot_evaluated_scores(model_names, model_colors, main="US 2021-22")
wis_plot_us_2223 <- flu_season_horizon_us |>
  dplyr::filter(season == "2022-2023") |>
  plot_evaluated_scores(model_names, model_colors, main="US 2022-23")
wis_plot_states_2122 <- flu_season_horizon_states |>
  dplyr::filter(season == "2021-2022") |>
  plot_evaluated_scores(model_names, model_colors, main="States 2021-22")
wis_plot_states_2223 <- flu_season_horizon_states |>
  dplyr::filter(season == "2022-2023") |>
  plot_evaluated_scores(model_names, model_colors, main="States 2022-23")

wis_plot_us_2122 + wis_plot_us_2223 + wis_plot_states_2122+wis_plot_states_2223 +
  plot_layout(ncol = 2, guides='collect') &
  theme(legend.position='bottom')
  
# forecast_date 
flu_forecast_date_horizon_us <- flu_scores_all |>
  evaluate_flu_scores(grouping_variables=c("horizon", "forecast_date"), baseline_name="Flusight-baseline", us_only=TRUE)

flu_forecast_date_horizon_states <- flu_scores_all |>
  evaluate_flu_scores(grouping_variables=c("horizon", "forecast_date"), baseline_name="Flusight-baseline", us_only=FALSE)
  

wis_date_plot_us1 + wis_date_plot_us4 + wis_date_plot_states1 + wis_date_plot_states4 +
  plot_layout(ncol = 2, guides='collect') &
  theme(legend.position='bottom')
