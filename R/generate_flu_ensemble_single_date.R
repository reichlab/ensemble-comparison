#' Compute ensemble model outputs as a quantile average, quantile median,
#' linear pool with normal tails, or linear pool with log normal tails
#' for each combination model task, output type, and output type id.
#' Component model forecasts for incident flu hospitalizations are queried 
#' from zoltar
#'
#' @param zoltar_connection a connection through which to access zoltar
#' @param project_url `character` string of the URL for these zoltar project 
#'   from which to query the component forecasts
#' @param origin_date `character` string that specifies the date on which
#'   the component forecasts were made that should be used for the ensemble
#' @param include_baseline `logical` that specifies whether to include the
#'   Flusight-baseline model in the ensemble.  Defaults to FALSE.
#' @param ensemble_type `character` string that specifies the ensembling method
#'   to be used on the flu forecasts. Can be "mean", "median", or "linear_pool".
#' @param dist_type `character` string that specifies the type of distribution 
#'   to use when calculating the tails for a linear pool ensemble. 
#'   Defaults to NULL. This argument is ignored for Vincentization.  
#' @param ... parameters that are passed to `distfromq::make_q_fun`, specifying
#'   details of how to estimate a quantile function from provided quantile levels 
#'   and quantile values.
#'
#' @return a `model_out_tbl` object of ensemble predictions for flu hospitalizations.
#' @export
#'
#' @examples
generate_flu_ensemble_single_date <- function(zoltar_connection, project_url,
                                              origin_date, include_baseline=FALSE,
                                              ensemble_type, dist_type=NULL, ...) {
  forecast_data <- zoltar_connection |>
    do_zoltar_query(project_url, query_type ="forecasts",
                    models = NULL,
                    units = NULL,
                    targets = NULL,
                    timezeros = origin_date,
                    types = "quantile")

  if (!include_baseline) {
    forecast_data <- forecast_data |>
      dplyr::filter(model != "Flusight-baseline")
  }

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

  # Ensemble forecasts
  if (ensemble_type == "linear_pool") {
    if (is.null(dist_type)) dist_type = "norm"
    lp_type <- ifelse(dist_type == "lnorm", "lognormal", "normal")
    ensemble_outputs <- forecast_data |>
      linear_pool(weights=NULL, weights_col_name=NULL,
                  model_id=paste("lp", lp_type, sep="-"),
                  task_id_cols = task_id_cols,
                  lower_tail_dist=dist_type,
                  upper_tail_dist=dist_type,
                  n_samples = 1e5)

  } else {
    ensemble_outputs <- forecast_data |>
      simple_ensemble(weights=NULL, weights_col_name=NULL,
                      agg_fun = ensemble_type,
                      model_id=paste(ensemble_type, "ensemble", sep="-"),
                      task_id_cols = task_id_cols)
  }
  
  ensemble_outputs <- mutate(value = ifelse(ensemble_outputs, value < 0, 0, value))
  return (ensemble_outputs)
}
