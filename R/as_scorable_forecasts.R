#' Reformat model outputs stored as a `model_output_tbl` class to that of
#' a `data.frame` that can be scored by `covidHubUtils::score_forecasts()` 
#'
#' @param model_outputs an object of class `model_output_tbl` with component
#'   model outputs (e.g., predictions).
#' @param reference_date_col `character` string of the name of the column
#'   containing the reference dates for the forecasts.  Defaults to "forecast_date"
#'
#' @return a `data.frame` of reformatted model outputs to be fed into `covidHubUtils::score_forecasts()`
#' @export
#'
#' @examples
as_scorable_forecasts <- function(model_outputs, reference_date_col="forecast_date", location_col="location", horizon_col="horizon", target_col="target", temp_res_col="temporal_resolution", target_end_date_col="target_end_date") {

  model_outputs <- model_outputs |> 
    dplyr::rename(model = model_id, type = output_type, quantile = output_type_id,
                  forecast_date = reference_date_col, location = location_col, target = target_col) 
  
  if (is.null(temp_res_col)) {
    model_outputs <- model_outputs |>
      tidyr::separate(target, sep=" ", convert=TRUE, into=c("temporal_resolution", "ahead", "target_variable"), extra="merge") 
  }
  
  if (is.null(target_end_date_col)) {
    model_outputs <- model_outputs |>
      dplyr::mutate(target_end_date=case_when(
        temporal_resolution %in% c("d", "day") ~ forecast_date + lubridate::days(horizon),
        temporal_resolution %in% c("w", "wk", "week") ~ forecast_date + lubridate::weeks(horizon),
        temporal_resolution %in% c("m", "mth", "mnth", "month") ~ forecast_date + lubridate::months(horizon),
        temporal_resolution %in% c("y", "yr", "year") ~ forecast_date + lubridate::years(horizon),
        .default = forecast_date), 
      .before = output_type) 
  }
                  
    dplyr::select(model, forecast_date, location, horizon, temporal_resolution, 
                  target_variable, target_end_date, type, quantile, value)
}