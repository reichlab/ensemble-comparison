library(tidyverse)
library(zoltr)
library(dplyr)
library(lubridate)
library(readr)
library(tidyr)
library(stringr)
library(hubUtils)
library(hubEnsembles)

# setwd("ensemble-comparison")
source("R/flu_truth_forecasts_functions.R")


# Establish connection
zoltar_connection <- new_connection()
zoltar_authenticate(zoltar_connection, Sys.getenv("Z_USERNAME"), Sys.getenv("Z_PASSWORD"))
# zoltar_connection

# Get projects and associated info
the_projects <- projects(zoltar_connection)
#str(the_projects)
project_url <- the_projects[the_projects$name == "CDC Influenza Hospitalization Forecasts", "url"]
the_project_info <- project_info(zoltar_connection, project_url)
# names(the_project_info)
# the_project_info$description
the_models <- models(zoltar_connection, project_url)
# str(the_models)

origin_dates <-  "2023-05-15"
task_id_cols <- c("forecast_date", "location", "horizon", "target")

# Get/format forecasts
forecast_data_hub <- zoltar_connection |>
  get_flu_forecasts_single_date(project_url, origin_dates) |>
  dplyr::filter(model_id != "Flusight-ensemble") 
forecast_data <- dplyr::rename(forecast_data_hub, model = model_id, type = output_type, quantile = output_type_id) 

flu_locations <- forecast_data$location |>
  unique() |> sort()

flu_quantiles <- forecast_data$quantile |>
  unique() |> sort()


# Flu dates
flu_dates_21_22 <- as.Date("2022-01-24") + weeks(0:21)
flu_dates_22_23 <- as.Date("2022-10-17") + weeks(0:30)
flu_dates_all <- c(flu_dates_21_22, flu_dates_22_23)


# Get truth data and baseline forecasts
flu_truth_all <- purrr::map_dfr(flu_dates_all, .f = function(dates_vector) {
  get_flu_truth_single_date(zoltar_connection, project_url, dates_vector) 
})

flu_baseline_all <- purrr::map_dfr(flu_dates_all, .f = function(dates_vector) {
  do_zoltar_query(zoltar_connection, project_url, 
                  query_type = "forecasts", models = "Flusight-baseline",
                  timezeros = dates_vector, types = "quantile") 
})

task_id_cols <- c("forecast_date", "location", "horizon", "target_long")

flu_baseline_all <- flu_baseline_all |>
    tidyr::separate(target, sep=" ", convert=TRUE, into=c("horizon", "target_long"), extra="merge") |>
    dplyr::rename(location = unit, forecast_date = timezero) |>
    as_model_out_tbl(model_id_col = "model",
                    output_type_col = "class",
                    output_type_id_col = "quantile",
                    value_col = "value",
                    sep = "-",
                    trim_to_task_ids = FALSE,
                    hub_con = NULL,
                    task_id_cols = task_id_cols,
                    remove_empty = TRUE) |>
    dplyr::rename(model = model_id, type = output_type, quantile = output_type_id) |>
    tidyr::separate(target_long, sep=" ", convert=TRUE, into=c("temporal_resolution", "ahead", "target_variable"), extra="merge") |>
    dplyr::mutate(value = ifelse(value < 0, 0, value)) |>
    dplyr::mutate(target_end_date=ceiling_date(forecast_date, "weeks")-days(1), .before = value) |>
    dplyr::select(model, forecast_date, location, horizon, temporal_resolution, 
                  target_variable, target_end_date, type, quantile, value)
                  

# Generate ensemble forecasts
flu_mean_21_22 <- purrr::map_dfr(flu_dates_21_22, .f = function(dates_vector) {
  generate_flu_ensemble_single_date(zoltar_connection, project_url,
                                    dates_vector, include_baseline=FALSE,
                                    ensemble_type="mean", dist_type=NULL) 
})
                                    
flu_mean_22_23 <- purrr::map_dfr(flu_dates_22_23, .f = function(dates_vector) {
  generate_flu_ensemble_single_date(zoltar_connection, project_url,
                                    dates_vector, include_baseline=FALSE,
                                    ensemble_type="mean", dist_type=NULL) 
})

flu_median_21_22 <- purrr::map_dfr(flu_dates_21_22, .f = function(dates_vector) {
  generate_flu_ensemble_single_date(zoltar_connection, project_url,
                                    dates_vector, include_baseline=FALSE,
                                    ensemble_type="median", dist_type=NULL) 
})
                                    
flu_median_22_23 <- purrr::map_dfr(flu_dates_22_23, .f = function(dates_vector) {
  generate_flu_ensemble_single_date(zoltar_connection, project_url,
                                    dates_vector, include_baseline=FALSE,
                                    ensemble_type="median", dist_type=NULL) 
})

flu_linear_pool_21_22 <- purrr::map_dfr(flu_dates_21_22, .f = function(dates_vector) {
  generate_flu_ensemble_single_date(zoltar_connection, project_url,
                                    dates_vector, include_baseline=FALSE,
                                    ensemble_type="linear_pool", dist_type=NULL) 
})
                    

lp_raw <- purrr::map_dfr(flu_dates_22_23, .f = function(dates_vector) {
  get_flu_forecasts_single_date(zoltar_connection, project_url, dates_vector) 
}); Sys.time()

lp_ensemble1 <-  lp_raw |>
  filter(forecast_date < "2023-02-01") |>
  dplyr::group_split(forecast_date) |>
  purrr::map_dfr(.f = function(split_forecasts) {
    generate_flu_ensemble(split_forecasts, include_baseline=FALSE, 
                          ensemble_type="linear_pool", dist_type=NULL)
  }) 
  
lp_ensemble2 <-  lp_raw |>
  filter(forecast_date > "2023-02-01") |>
  dplyr::group_split(forecast_date) |>
  purrr::map_dfr(.f = function(split_forecasts) {
    generate_flu_ensemble(split_forecasts, include_baseline=FALSE, 

                          ensemble_type="linear_pool", dist_type=NULL)
  }) 

flu_linear_pool_22_23 <- rbind(lp_ensemble1, lp_ensemble2)


flu_lp_lognorm_21_22 <- purrr::map_dfr(flu_dates_21_22, .f = function(dates_vector) {
  generate_flu_ensemble_single_date(zoltar_connection, project_url,
                                    dates_vector, include_baseline=FALSE,
                                    ensemble_type="linear_pool", tail_dist="lnorm") 
})
                    
lp_lognorm1 <-  lp_raw |>
  filter(forecast_date %in% flu_dates_22_23[1:10]) |>
  dplyr::group_split(forecast_date) |>
  purrr::map_dfr(.f = function(split_forecasts) {
    generate_flu_ensemble(model_outputs=split_forecasts, include_baseline=FALSE, 
                          ensemble_type="linear_pool", tail_dist="lnorm")
  }) 
readr::write_rds(lp_lognorm1, "data/flu_lp_lognorm1.rds", "xz", compression = 9L)

lp_lognorm2 <-  lp_raw |>
  filter(forecast_date %in% flu_dates_22_23[11:20]) |>
  dplyr::group_split(forecast_date) |>
  purrr::map_dfr(.f = function(split_forecasts) {
    generate_flu_ensemble(split_forecasts, include_baseline=FALSE, 
                          ensemble_type="linear_pool", tail_dist="lnorm")
  }) 
readr::write_rds(lp_lognorm1, "data/flu_lp_lognorm1.rds", "xz", compression = 9L)
  
lp_lognorm3 <-  lp_raw |>
  filter(forecast_date %in% flu_dates_22_23[21:31]) |>
  dplyr::group_split(forecast_date) |>
  purrr::map_dfr(.f = function(split_forecasts) {
    generate_flu_ensemble(split_forecasts, include_baseline=FALSE, 
                          ensemble_type="linear_pool", tail_dist="lnorm")
  }) 

flu_lp_lognorm_22_23 <- rbind(lp_lognorm1, lp_lognorm2)
View(flu_lp_lognorm_21_22)
readr::write_rds(flu_lp_lognorm_21_22, "data/flu_lp_lognorm-ensemble_21-22.rds", "xz", compression = 9L)
readr::write_rds(flu_lp_lognorm_22_23, "data/flu_lp_lognorm-ensemble_22-23.rds", "xz", compression = 9L)


  
