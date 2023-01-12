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
#'   This function expects `dat` to contain the following fields:
#'   * `scenario`: a character field used to identify the name of the landscape
#'   being examined; at least one should be called `'baseline'`
#'   * either `area` (resulting from [sum_habitat()]) or `SCORE_TOTAL` and
#'   `SCORE_TOTAL_SE` (resulting from [sum_metrics()]): numeric fields representing the total
#'   landscape-level values for each metric and associated uncertainty
#'
#' @param dat tibble; see Details
#'
#' @return tibble
#' @seealso [sum_habitat()], [sum_metrics()]
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' # See vignette

sum_change = function(dat) {
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
                        net_change_se = sqrt(.data$SCENARIO_SE^2 + .data$BASELINE_SE^2))
  }

  return(res)

  }
