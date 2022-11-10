#' Summarize net change between scenario and baseline landscapes
#'
#' Calculate the difference in the landscape-level metric totals between a
#' baseline landscape and one or more scenario landscapes
#'
#' @details This function takes a tibble containing the metric scores for
#'   multiple landscapes, including those produced by [sum_habitat()] or
#'   [sum_metrics()], with or without scores by ZONE within the entire
#'   landscape. Scores for a baseline landscape are subtracted from scores for
#'   one or more scenario landscapes to calculate a net change and the
#'   proportion and percent change from baseline, along with propagation of
#'   uncertainty.
#'
#'   This function expects `scoredat` to contain the following fields, in
#'   alignment with the output of [sum_habitat()] and [sum_metrics()]:
#'   * scenario: a character field used to identify the name of the landscape
#'   being examined; at least one should be called `'baseline'`
#'   * SCORE_TOTAL, SCORE_TOTAL_SE: numeric fields representing the total
#'   landscape-level values for each metric and associated uncertainty
#'
#' @param scoredat tibble; see Details
#'
#' @return tibble
#' @seealso [sum_habitat()], [sum_metrics()]
#' @export
#'
#' @examples
#' # See vignette

sum_change = function(scoredat) {
  dplyr::left_join(
    scoredat %>% dplyr::filter(scenario != 'baseline') %>%
      dplyr::rename(SCENARIO = SCORE_TOTAL, SCENARIO_SE = SCORE_TOTAL_SE),
    scoredat %>% dplyr::filter(scenario == 'baseline') %>%
      dplyr::rename(BASELINE = SCORE_TOTAL, BASELINE_SE = SCORE_TOTAL_SE) %>%
      dplyr::select(-scenario)) %>%
    dplyr::mutate(net_change = SCENARIO - BASELINE,
           net_change_se = sqrt(SCENARIO_SE^2 + BASELINE_SE^2),
           change_prop = net_change/BASELINE,
           change_prop_se = abs(change_prop) * sqrt((net_change_se/net_change)^2 + (BASELINE_SE/BASELINE)^2),
           change_pct = change_prop * 100,
           change_pct_se = change_prop_se * 100)
  }
