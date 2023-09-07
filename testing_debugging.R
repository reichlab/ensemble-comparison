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
forecast_data_hub <- get_flu_forecasts_single_date(zoltar_connection, project_url, origin_dates) |>
  dplyr::filter(model_id != "Flusight-ensemble") 
forecast_data <- dplyr::rename(forecast_data_hub, model = model_id, type = output_type, quantile = output_type_id) 


# Function testing
mean_ensemble <- forecast_data_hub |>
  filter(model_id != "Flusight-baseline") |>
  hubEnsembles::simple_ensemble(agg_fun = "mean", model_id="mean-ensemble") |>
  arrange(unit)
  
median_ensemble <- forecast_data_hub |>
  filter(model_id != "Flusight-baseline") |>
  hubEnsembles::simple_ensemble(agg_fun = "median", model_id="median-ensemble") |>
  arrange(location)

lp_normal <- forecast_data_hub |>
  filter(model_id != "Flusight-baseline") |>
  hubEnsembles::linear_pool(n_samples = 1e5, model_id="lp-normal") |>
  arrange(location)

# FIX ME:  For dist = 'lnorm', all qs must be positive
lp_lognormal <- forecast_data_hub |>
  filter(model_id != "Flusight-baseline") |>
  mutate(value = ifelse(value < 0, 0, value)) |>
  hubEnsembles::linear_pool(n_samples = 1e2, model_id="lp-lognormal", tail_dist="lnorm") |>
  arrange(location)

actual_mean <- generate_flu_ensemble_single_date(zoltar_connection, project_url,
                                  origin_dates, include_baseline=FALSE,
                                  ensemble_type="mean", tail_dist=NULL) 
                                 
actual_median <- generate_flu_ensemble_single_date(zoltar_connection, project_url,
                                  origin_dates, include_baseline=FALSE,
                                  ensemble_type="median", tail_dist=NULL) 

actual_lp_norm <- generate_flu_ensemble_single_date(zoltar_connection, project_url,
                                  origin_dates, include_baseline=FALSE,
                                  ensemble_type="linear_pool", tail_dist=NULL) 
                                  
# FIX ME:  For dist = 'lnorm', all qs must be positive
# actual_lp_lognorm <- generate_flu_ensemble_single_date(zoltar_connection, project_url,
#                                   origin_dates, include_baseline=FALSE,
#                                   ensemble_type="linear_pool", tail_dist="lnorm") 
                                  
expect_equal(select(mean_ensemble, -season), actual_mean)
expect_equal(select(median_ensemble, -season), actual_median)
expect_equal(select(lp_normal, -season), actual_lp_norm)


# Flu dates
flu_dates_21_22 <- as.Date("2022-01-24") + weeks(0:21)
flu_dates_22_23 <- as.Date("2022-10-17") + weeks(0:30)
flu_dates_all <- c(flu_dates_21_22, flu_dates_22_23)

task_id_cols <- c("forecast_date", "location", "horizon", "target_long")

                  
# Debugging work
toy_data <- generate_flu_ensemble_single_date(zoltar_connection, project_url,
                                  flu_dates_21_22[6], include_baseline=FALSE,
                                  ensemble_type="linear_pool", dist_type=NULL) 

test_data <- zoltar_connection |>
  do_zoltar_query(project_url, query_type ="forecasts",
                  models = NULL,
                  units = NULL,
                  targets = NULL,
                  timezeros = flu_dates_21_22[6],
                  types = "quantile") |>
  dplyr::filter(model != "Flusight-ensemble") |>
  tidyr::separate(target, sep=2, convert=TRUE, into=c("horizon", "target")) |>
  as_model_out_tbl(model_id_col = "model",
                  output_type_col = "class",
                  output_type_id_col = "quantile",
                  value_col = "value",
                  sep = "-",
                  trim_to_task_ids = FALSE,
                  hub_con = NULL,
                  task_id_cols = task_id_cols,
                  remove_empty = TRUE) 

test_data |>
    dplyr::filter(model_id == "Flusight-baseline", 
                  timezero == as.Date("2022-02-28"), unit=="15", horizon==4) |>
#    dplyr::mutate(value = round(value, 10)) |>
    pull(value)

  quantile_levels <- unique(test_data$output_type_id)
  agg_args <- c(list(x = quote(.data[["pred_qs"]]), probs = quantile_levels))

test_data |>
#    dplyr::mutate(value = ifelse(value < 0, 0, value)) |>
    tidyr::separate(target, sep=" ", convert=TRUE, into=c("temporal_resolution", "ahead", "target_long"), extra="merge") |>
    dplyr::rename(location = unit, forecast_date = timezero) |>
    dplyr::group_by(model_id, dplyr::across(dplyr::all_of(task_id_cols))) |>
    dplyr::summarize(
      pred_qs = list(distfromq::make_q_fn(
        ps = output_type_id,
        qs = value,
        dup_tol = 1e-02,
        zero_tol = 1e-02,
        tail_dist = "lnorm")(seq(from = 0, to = 1, length.out = 1e3 + 2)[2:1e3])),
      .groups = "drop") |>
    tidyr::unnest(pred_qs) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(task_id_cols))) |>
    dplyr::summarize(
      output_type_id= list(quantile_levels),
      value = list(do.call(Hmisc::wtd.quantile, args = agg_args)),
      .groups = "drop") |>
    tidyr::unnest(cols = tidyselect::all_of(c("output_type_id", "value"))) |>
    dplyr::mutate(output_type = "quantile", .before = output_type_id) |>
    dplyr::ungroup() 
    
rlang::last_trace(drop=FALSE)
