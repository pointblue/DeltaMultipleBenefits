#' Summarize total metric scores
#'
#' Calculate the landscape-level sum of metrics for a set of landscape rasters
#'
#' @details This function will combine the landscape-specific estimates of the
#'   total area of each land cover class provided by `areadat` (optionally also
#'   by zone; see [sum_landcover()]) with per-unit-area metrics associated with
#'   each land cover class as provided in `metricdat`. The two data sets are
#'   joined by any fields in common, which should at minimum include a field
#'   containing the land cover classes.
#'
#'   For most metrics, scores are calculated by multiplying the total area by
#'   the corresponding per-unit-area metric and summing over the entire
#'   landscape. However, for Annual Wages associated with agricultural jobs, a
#'   weighted average is instead produced, based on the average wage for an
#'   agricultural job associated with each land cover class and the proportion
#'   of the landscape that has an associated wage (i.e., only crop classes) that
#'   is made up by that land cover class. In addition, for the metrics
#'   representing the Climate Change Resilience category, the overall landscape
#'   average score is produced.
#'
#'   This function expects `metricdat` to contain the following fields:
#'   * class: a character field containing the land cover classes; may be called
#'   anything, but should match with a corresponding field in `areadat`
#'   * METRIC: character field used to define specific metrics; expects a METRIC
#'   called "Annual Wages"
#'   * SCORE_MEAN, SCORE_SE: numeric fields containing the specific values for
#'   each METRIC and an estimate of uncertainty
#'   * METRIC_CATEGORY, UNIT: optional additional character
#'   fields useful for grouping METRICS and tracking units; may contain
#'   anything, will be retained in output. If UNIT is present, "/ha" will be removed from the results before returning, as a reminder that the resulting total scores are no longer per-hectare.
#'
#'   This function also expects `areadat` to contain the following fields:
#'   * scenario: character field used to identify the name of the landscape being
#'   examined
#'   * class: as above, a character field containing the land cover classes; may
#'   be called anything, but should match with a corresponding field in
#'   `metricdat`
#'   * area: numeric field containing the total area of each land cover class.
#'   Take care that the units in which the area field was calculated correspond
#'   to the UNITs by which the per-unit-area METRICS in `metricdat` were defined
#'   (often ha). (See [sum_landcover()].)
#'   * ZONE: optional character field identifying the name of a zone within which the
#'   area of land covers was estimated, such as by running [sum_landcover()]
#'   with `zones` provided; if ZONE is present, the output will be
#'   summarized by zone.
#'
#' @param metricdat tibble; See Details
#' @param areadat tibble; See Details
#'
#' @return tibble
#' @seealso [sum_landcover()]
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' # See vignette

sum_metrics = function(metricdat, areadat) {

  dat_join = dplyr::full_join(areadat, metricdat) %>%
    dplyr::mutate(
      # retain mean value for Annual Wages for now, otherwise multiply by area
      # for total score
      SCORE_TOTAL = dplyr::if_else(.data$METRIC == 'Annual Wages',
                                   .data$SCORE_MEAN,
                                   .data$area * .data$SCORE_MEAN),
      # propagate error: multiplication by a constant
      SCORE_TOTAL_SE = dplyr::if_else(.data$METRIC == 'Annual Wages',
                                      .data$SCORE_SE,
                                      .data$area * .data$SCORE_SE)
      )

  # check for missing land covers (since we did a full join)
  tmp = dat_join %>% dplyr::filter(is.na(.data$area))
  if (nrow(tmp) > 0) {
    warning('Area estimates missing from "areadat" for one or more land cover classes in "metricdat"')
  }

  res = dplyr::bind_rows(
    # for all but annual wages, sum over all land cover classes:
    dat_join %>%
      dplyr::filter(.data$METRIC != 'Annual Wages') %>%
      dplyr::group_by(
        dplyr::across(
          dplyr::any_of(
            c('scenario', 'ZONE', 'METRIC_CATEGORY', 'METRIC_SUBTYPE',
              'METRIC', 'UNIT')))) %>%
      dplyr::summarize(
        area = sum(.data$area, na.rm = TRUE),
        SCORE_TOTAL = sum(.data$SCORE_TOTAL, na.rm = TRUE),
        SCORE_TOTAL_SE = sqrt(sum(.data$SCORE_TOTAL_SE^2, na.rm = TRUE)),
        .groups = 'drop') %>%
      # for climate change resilience, divide by the area
      dplyr::mutate(
        dplyr::across(
          c(.data$SCORE_TOTAL, .data$SCORE_TOTAL_SE),
          ~dplyr::if_else(.data$METRIC_CATEGORY == 'Climate Change Resilience',
                          ./.data$area,
                          .))) %>%
      dplyr::select(-.data$area),
    # for annual wages: multiply the average wage per-landcover by the
    # proportion of the total ag landscape made up by that land cover
    dat_join %>%
      dplyr::filter(.data$METRIC == 'Annual Wages' & .data$SCORE_TOTAL > 0) %>%
      dplyr::group_by(
        dplyr::across(
          dplyr::any_of(
            c('scenario', 'ZONE', 'METRIC_CATEGORY', 'METRIC_SUBTYPE',
              'METRIC', 'UNIT')))) %>%
      dplyr::mutate(
        area_ag_total = sum(.data$area, na.rm = TRUE),
        area_prop = .data$area / .data$area_ag_total) %>%
      dplyr::summarize(
        SCORE_TOTAL = sum(.data$SCORE_TOTAL * .data$area_prop, na.rm = TRUE),
        SCORE_TOTAL_SE = sqrt(sum((.data$SCORE_TOTAL_SE * .data$area_prop)^2,
                                  na.rm = TRUE)),
        .groups = 'drop')
  )

  if ('UNIT' %in% names(res)) {
    res = res %>%
      dplyr::mutate(UNIT = gsub('/ha', '', .data$UNIT))
  }
  return(res)
}
