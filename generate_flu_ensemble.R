library(tidyverse)
library(lubridate)
library(readr)
library(stringr)
library(hubEnsembles)
library(hubUtils)

reference_date <- lubridate::ceiling_date(Sys.Date(), "week") - days(1)
task_id_cols <- c("reference_date", "location", "horizon", "target", "target_end_date")
  
hub_path <- "../FluSight-forecast-hub"
hub_con <- connect_hub(hub_path) 
hub_con |>
  dplyr::filter(
    reference_date == current_ref_date, 
    stringr::str_detect(model, "Flusight", negate=TRUE) # remove baseline and ensembles
  ) |> 
  dplyr::collect() |>
  as_model_out_tbl() 


# generate median ensemble
median_name <- "Flusight-median"
median_ensemble_outputs <- current_forecasts |>
  hubEnsembles::simple_ensemble(
    agg_fun="median", 
    model_id=median_name, 
    task_id_cols=task_id_cols
  ) |>
  dplyr::mutate(value = ifelse(value < 0, 0, value)) |>
  dplyr::select(-model_id)

median_ensemble_path <- paste(hub_path, "/model-outputs/", median_name, "/", reference_date, "-", median_name, ".csv", sep="") 
readr::write_csv(median_ensemble_outputs, median_ensemble_path)
  

# generate linear pool
lop_norm_name <- "Flusight-lop_norm"
lop_normal_outputs <- current_forecasts |>
  hubEnsembles::linear_pool(
    model_id=lop_norm_name, 
    task_id_cols=task_id_cols
  ) |>
  dplyr::mutate(value = ifelse(value < 0, 0, value)) |>
  dplyr::select(-model)

lop_norm_ensemble_path <- paste(hub_path, "/model-outputs/", lop_norm_name, "/", reference_date, "-", lop_norm_name, ".csv", sep="") 
readr::write_csv(lop_norm_outputs, lop_norm_path)

