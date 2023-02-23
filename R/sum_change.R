#'Summarize net change between scenario and baseline landscapes
#'
#'Calculate the difference in the landscape-level metric totals between a
#'baseline landscape and one or more scenario landscapes
#'
#'@details This function expects `dat` to contain the following fields:
#'   * `scenario`: a character field used to identify the name of the landscape
#'  being examined; one of these must be called `'baseline'` and all others will
#'  be treated as alternate scenarios for comparison with the baseline
#'   * either `area` or `SCORE_TOTAL`: numeric fields representing the total
#'  landscape-level values for each metric, such as those produced by
#'  [sum_landcover()] or [sum_metrics()].
#'
#'  The scores for each scenario are aligned with corresponding scores for the
#'  baseline landscape based on any other common fields (e.g. `ZONE`,
#'  `METRIC_CATEGORY`) and the net difference is calculated as the scenario
#'  score minus the baseline score. The function returns the original baseline
#'  and scenario total landscape scores for each metric and scenario (renamed as
#'  `BASELINE` and `SCENARIO`), along with `net_change`.
#'
#'  If `SCORE_TOTAL_SE` is also provided in `dat`, representing the uncertainty
#'  in the `SCORE_TOTAL`, the uncertainty in the difference (`net_change_se`) is
#'  also calculated as: `sqrt(baseline_se^2 + scenario_se^2)` where
#'  `baseline_se` and `scenario_se` represent the `SCORE_TOTAL_SE` for the
#'  baseline and scenario landscapes, respectively. In addition, the coverage
#'  factor `k` is used to estimate expanded uncertainty (`U`), or the interval
#'  within which a large fraction of the distribution of values could be
#'  reasonably expected. The appropriate value for `k` depends on the level of
#'  confidence required, the number of observations on which the uncertainty is
#'  based, and any knowledge of the underlying distributions of the estimates.
#'  Where the distributions concerned are normal, and for most purposes, a value
#'  of 2 is recommended  to give an interval containing approximately 95% of the
#'  distribution of values. In this case, the function also returns the original
#'  uncertainty estimates for the baseline and scenario (renamed as
#'  `BASELINE_SE` and `SCENARIO_SE`), `net_change_se`, the expanded uncertainty
#'  estimate `U` and corresponding `lcl` and `ucl`, as well as a `z` score equal
#'  to `abs(net_change/net_change_se)`.
#'
#'@param dat tibble; see Details
#'@param k coverage factor; see Details
#'
#'@return tibble containing fields `BASELINE`, `SCENARIO`, `net_change`, and
#'  optionally `net_change_se`, `U`, `lcl`, `ucl`, and `z`; see Details
#'@seealso [sum_habitat()], [sum_metrics()]
#'@importFrom magrittr %>%
#'@importFrom rlang .data
#'@export
#'
#' @examples
#' # See vignette

sum_change = function(dat, k = 2) {
  dat = dplyr::rename_with(dat, ~gsub('area|SCORE_TOTAL', 'value', .x))

  res = dplyr::left_join(
    dat %>% dplyr::filter(.data$scenario != 'baseline') %>%
      dplyr::rename_with(~gsub('value', 'SCENARIO', .x)),
    dat %>% dplyr::filter(.data$scenario == 'baseline') %>%
      dplyr::rename_with(~gsub('value', 'BASELINE', .x)) %>%
      dplyr::select(-.data$scenario)) %>%
    dplyr::mutate(net_change = .data$SCENARIO - .data$BASELINE)

  if ('value_SE' %in% names(dat)) {
    res = dplyr::mutate(res,
                        net_change_se = sqrt(.data$SCENARIO_SE^2 + .data$BASELINE_SE^2),
                        U = .data$net_change_se * k,
                        lcl = .data$net_change - .data$U,
                        ucl = .data$net_change + .data$U,
                        z = abs(.data$net_change / .data$net_change_se))
  }

  return(res)

  }
