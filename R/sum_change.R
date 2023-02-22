#'Summarize net change between scenario and baseline landscapes
#'
#'Calculate the difference in the landscape-level metric totals between a
#'baseline landscape and one or more scenario landscapes
#'
#'@details This function expects `dat` to contain the following fields:
#'   * `scenario`: a character field used to identify the name of the landscape
#'  being examined; at least one should be called `'baseline'`
#'   * `SCORE_TOTAL` and (optionally) `SCORE_TOTAL_SE`: numeric fields
#'  representing the total landscape-level values for each metric and associated
#'  uncertainty, such as those produced by [sum_habitat()] or [sum_metrics()].
#'
#'  The function will align the scores for each scenario with corresponding
#'  scores for the baseline landscape based on any other common fields (e.g.
#'  `ZONE`, `METRIC_CATEGORY`), and then calculate the net difference as the
#'  scenario score minus the baseline score. The uncertainty in the underlying
#'  total landscape-level scores for each metric is accounted for by estimating
#'  the uncertainty in the difference as: `sqrt(baseline_se^2 + scenario_se^2)`
#'
#'  The coverage factor `k` is used to estimate expanded uncertainty (`U`), or
#'  the interval within which a large fraction of the distribution of values
#'  could be reasonably expected. The appropriate value for `k` depends on the
#'  level of confidence required, the number of observations on which the
#'  uncertainty is based, and any knowledge of the underlying distributions of
#'  the estimates. Where the distributions concerned are normal, and for most
#'  purposes, a value of 2 is recommended  to give an interval containing
#'  approximately 95% of the distribution of values.
#'
#'  The function returns the original baseline and scenario total landscape
#'  scores for each metric and scenario (renamed as `BASELINE`, `BASELINE_SE`,
#'  `SCENARIO`, AND `SCENARIO_SE`), along with `net_change` and `net_change_se`,
#'  the expanded uncertainty estimate `U`, and corresponding `lcl` and `ucl`, as
#'  well as a `z` score equal to `abs(net_change/net_change_se)`.
#'
#'@param dat tibble; see Details
#'@param k coverage factor; see Details
#'
#'@return tibble containing fields `net_change`, `net_change_se`, `U`, `lcl`,
#'  `ucl`, and `z`
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
                        U = net_change_se * k,
                        lcl = net_change - U,
                        ucl = net_change + U,
                        z = abs(net_change / net_change_se))
  }

  return(res)

  }
