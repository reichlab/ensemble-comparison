library(tidyverse)
library(lubridate)
library(readr)
library(stringr)
library(hubEnsembles)

# getwd()
# flusight_truth_path <- "../Flusight-forecast-data/data-truth/truth-Incident Hospitalizations.csv"
# current_truth <- readr::read_csv(flusight_truth_path)

# reference_date <- lubridate::ceiling_date(Sys.Date(), "week") - days(1)
reference_date <- "2023-05-08"
flusight_data_path <- "../Flusight-forecast-data/data-forecasts"
model_names <- list.dirs(flusight_data_path, full.names=FALSE) 
current_forecast_files <- list.files(flusight_data_path, full.names=TRUE, recursive=TRUE, pattern=reference_date) 

current_forecasts_raw <- purrr::map_dfr(current_forecast_files, .f=function(path) {
  name_start <- stringr::str_locate(path, reference_date)[1, 1] + 11
  model_name <- stringr::str_sub(path, start=name_start, end=stringr::str_length(path) - 4)
  readr::read_csv(path) |>
    dplyr::mutate(model = model_name, .before = 1)
}) 

# reference_date will replace forecast_date
task_id_cols <- c("forecast_date", "location", "horizon", "target")
  
current_forecasts_formatted <- current_forecasts_raw |>
  dplyr::filter(str_detect(model, "Flusight", negate=TRUE)) |> # remove baseline and ensemble
  tidyr::separate(target, sep=" ", convert=TRUE, into=c("horizon", "target"), extra="merge") |>
  # May need to merge `model` and `team` columns into `model_id` column
  as_model_out_tbl(model_id_col = "model",
                  output_type_col = "type",
                  output_type_id_col = "quantile",
                  value_col = "value",
                  sep = "-",
                  trim_to_task_ids = FALSE,
                  hub_con = NULL,
                  task_id_cols = task_id_cols,
                  remove_empty = TRUE) #|>

names(current_forecasts_formatted)

# generate median ensemble
median_ensemble_outputs <- current_forecasts_formatted |>
  hubEnsembles::simple_ensemble(agg_fun="median", model_id="Flusight-median", task_id_cols=task_id_cols) |>
  dplyr::mutate(value = ifelse(value < 0, 0, value)) |>
  dplyr::select(-model)

median_ensemble_path <- paste(flusight_data_path, "/Flusight-median/", reference_date, "-Flusight-median.csv", sep="") 
readr::write_csv(median_ensemble_outputs, median_ensemble_path)
  
# generate linear pool
lop_normal_outputs <- current_forecasts_formatted |>
  hubEnsembles::linear_pool(model_id="Flusight-lop_norm", tail_dist="norm", task_id_cols=task_id_cols) |>
  dplyr::mutate(value = ifelse(value < 0, 0, value)) |>
  dplyr::select(-model)

lop_normal_path <- paste(flusight_data_path, "/Flusight-lop_norm/", reference_date, "-Flusight-lop_norm.csv", sep="") 
readr::write_csv(lop_normal_outputs, lop_normal_path)
