#' Evaluate forecasts (from scores) across different evaluation groupings
#'
#' @param scores A data frame of forecast scores to be summarized. Must have columns that match the output from `covidHubUtils::score_forecasts()` and include scores for a baseline model.
#' @param grouping_variables A character vector specifying the type of grouping to be used in evaluation. Options are `season`, `horizon`, `forecast_week`, `location`.
#' @param baseline_name A character string specifying the name of the baseline model to calculate relative metrics against
#' @param us_only A boolean specifying whether to summarize metrics for the us national level only or just states
#'
#' @return A data frame of summarized forecast score metrics across all provided forecasts
#' @export
#'
#' @examples
evaluate_flu_scores <- function(scores, grouping_variables, baseline_name, us_only=FALSE) {
  if (isFALSE("location" %in% grouping_variables)) {
    if (us_only) {
      scores <- filter(scores, location=="US")
    } else {
      scores <- filter(scores, location!="US")
    }
  }
  
  if ("season" %in% grouping_variables & isFALSE("season" %in% names(scores))) {
    scores <- scores |>
      dplyr::mutate(season = ifelse(forecast_date < as.Date("2022-08-01"), 
                                    "2021-2022", "2022-2023"))
  }
  
  summarized_scores_all <- scores |>
    dplyr::group_by(model, dplyr::across(dplyr::all_of(grouping_variables))) |>
    dplyr::summarize(wis = mean(wis), mae = mean(abs_error), 
                     cov50 = mean(coverage_50), cov95 = mean(coverage_95))
                     
  summarized_scores_baseline <- scores |>
    dplyr::filter(model==baseline_name) |>
    dplyr::group_by(model, dplyr::across(dplyr::all_of(grouping_variables))) |>
    dplyr::summarize(base_wis = mean(wis), base_mae = mean(abs_error)) |>
    dplyr::ungroup() |>
    dplyr::select(-model) 
    
  if (is.null(grouping_variables)) {
    summarized_scores_all <- summarized_scores_all |>
      dplyr::mutate(base_wis = unique(summarized_scores_baseline$base_wis),
                    base_mae = unique(summarized_scores_baseline$base_mae))
  } else {
    summarized_scores_all <- summarized_scores_all |>
      dplyr::left_join(summarized_scores_baseline, by = grouping_variables) 
    
  }
  
  summarized_scores_all |>
    dplyr::mutate(rwis = case_when(base_wis != 0 ~ wis/base_wis,
                                    base_wis == 0 & wis == 0 ~ 1,
                                    base_wis == 0 & wis != 0 ~ Inf),
                  rmae = case_when(base_mae != 0 ~ mae/base_mae,
                                    base_mae == 0 & mae == 0 ~ 1,
                                    base_mae == 0 & mae != 0 ~ Inf)) |>
    dplyr::select(-base_wis, -base_mae) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping_variables))) |>
    dplyr::arrange(wis, .by_group=TRUE)

}

#' Plot summarized metrics against horizon week
#'
#' @param summarized_scores A data frame of summarized scores. Must contain one row per model and horizon week combination plus a `horizon` column
#' @param model_names An ordered vector of model names
#' @param model_colors An ordered vector of model colors. Must match with `model_names` order
#' @param y_var A string specifying which metric to plot as the y-variable
#' @param main A string specifying the plot title
#'
#' @return A scatter plot (with observations connected by lines) of the specified summary metric vs horizon week
#' @export
#'
#' @examples
plot_evaluated_scores <- function(summarized_scores, model_names, model_colors, y_var="wis", main) {

  if (y_var == "wis") {
    gg <- ggplot(summarized_scores, mapping=aes(x=horizon, y=wis, group=model)) #+
      coord_cartesian(ylim = c(0, median(filter(summarized_scores, horizon==4)$wis)*1.5))
  } else if (y_var == "mae") {
    gg <- ggplot(summarized_scores, mapping=aes(x=horizon, y=mae, group=model)) +
      coord_cartesian(ylim = c(0, median(filter(summarized_scores, horizon==4)$mae)*1.5))
  } else if (y_var == "cov95") {
    gg <- ggplot(summarized_scores, mapping=aes(x=horizon, y=cov95, group=model)) +
      geom_hline(aes(yintercept=0.95))
  } else if (y_var == "cov50") {
    gg <- ggplot(summarized_scores, mapping=aes(x=horizon, y=cov95, group=model)) +
      geom_hline(aes(yintercept=0.50))
  }

  gg +
    geom_point(mapping=aes(col=model), alpha = 0.8) +
    geom_line(mapping=aes(col=model), alpha = 0.8) +
    scale_color_manual(breaks = model_names, values = model_colors) +
    labs(title=main, x="horizon week", y=paste("average", y_var)) +
    theme_bw()
}
