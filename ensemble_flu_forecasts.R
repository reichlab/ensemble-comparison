library(tidyverse)
library(zoltr)
library(dplyr)
library(lubridate)
library(readr)
library(tidyr)
library(stringr)
library(hubUtils)
library(hubEnsembles)

# parallelize code
library(doParallel)
num_cores <- detectCores(logical=TRUE)
cl <- makeCluster(num_cores-1)
registerDoParallel(cl)

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
# Query forecasts
forecast_data <- zoltar_connection |> 
  do_zoltar_query(project_url, query_type ="forecasts", 
#                  models = "docs forecast model", 
#                  units = c("loc1", "loc2"), 
#                  targets = "week ahead incident hospitalizations",
                  timezeros = origin_dates, 
                  types = "quantile")

task_id_cols <- c("timezero", "unit", "horizon", "target")
# Format forecasts
forecast_data <- forecast_data |>
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
forecast_data

mean_ensemble <- forecast_data |>
  filter(model_id != "Flusight-baseline") |>
  hubEnsembles::simple_ensemble(agg_fun = "mean", model_id="mean-ensemble") |>
  arrange(unit)
  
median_ensemble <- forecast_data |>
  filter(model_id != "Flusight-baseline") |>
  hubEnsembles::simple_ensemble(agg_fun = "median", model_id="median-ensemble") |>
  arrange(unit)

lp_normal <- forecast_data |>
  filter(model_id != "Flusight-baseline") |>
  hubEnsembles::linear_pool(n_samples = 1e5, model_id="lp-normal") |>
  arrange(unit)
  
ensemble_forecasts <- zoltar_connection |> 
  do_zoltar_query(project_url, query_type ="forecasts", 
                  models = "Flusight-ensemble", 
                  timezeros = origin_dates, 
                  types = "quantile") |>
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
ensemble_forecasts <-arrange(ensemble_forecasts,unit)

#"forecast_date","target","target_end_date","location","type","quantile","value"

#  dplyr::mutate(target_end_date=ceiling_date(timezero, "weeks")-days(1))

testthat::expect_equal(mean_ensemble,ensemble_forecasts)
testthat::expect_equal(median_ensemble,ensemble_forecasts)

combined_ensembles <- ensemble_forecasts |> 
  rbind(mean_ensemble, median_ensemble)


source("R/generate_flu_ensemble_single_date.R")

actual_mean <- generate_flu_ensemble_single_date(zoltar_connection, project_url,
                                  origin_dates, include_baseline=FALSE,
                                  ensemble_type="mean", dist_type=NULL) 
                                  
actual_median <- generate_flu_ensemble_single_date(zoltar_connection, project_url,
                                  origin_dates, include_baseline=FALSE,
                                  ensemble_type="median", dist_type=NULL) 

actual_lp_norm <- generate_flu_ensemble_single_date(zoltar_connection, project_url,
                                  origin_dates, include_baseline=FALSE,
                                  ensemble_type="linear_pool", dist_type=NULL) 
                                  
expect_equal(select(mean_ensemble, -season), actual_mean)
expect_equal(select(median_ensemble, -season), actual_median)
expect_equal(select(lp_normal, -season), actual_lp_norm)



