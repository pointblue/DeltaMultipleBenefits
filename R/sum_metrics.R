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
#'   landscape. However, one exception is for Annual Wages associated with
#'   agricultural jobs. In this case, a weighted average is produced, based on
#'   the average wage for an agricultural job associated with each land cover
#'   class and the proportion of the landscape that has an associated wage
#'   (i.e., only crop classes) that is made up by that land cover class.
#'
#'   This function expects `metricdat` to contain the following fields:
#'   * class: a character field containing the land cover classes; may be
#'   called anything, but should match with a corresponding field in `areadat`
#'   * METRIC: character field used to define specific metrics; expects a
#'   METRIC called "Annual Wages"
#'   * SCORE_MEAN, SCORE_SE: numeric fields containing the specific values for
#'   each METRIC and `class` and an estimate of uncertainty
#'   * METRIC_CATEGORY, METRIC_SUBTYPE, UNIT: optional additional character
#'   fields useful for grouping METRICS and tracking units; may contain
#'   anything, will be retained in output
#'
#'   This function also expects `areadat` to contain the following fields:
#'   * scenario: character field used to identify the name of the landscape
#'   being examined
#'   * class: as above, a character field containing the land cover classes;
#'   may be called anything, but should match with a corresponding field in
#'   `metricadat`
#'   * area: numeric field containing the total area of each land cover class.
#'   Take care that the units in which the area field was calculated correspond
#'   to the UNITs by which the per-unit-area METRICS in `metricdat` were defined
#'   (often ha). (See [sum_landcover()].)
#'   * ZONE: optional character field identifying the name of a zone within
#'   which the area of land covers was estimated, such as by running
#'   [sum_landcover()] with a `zonepath` provided; if ZONE is present, the
#'   output will be summarized by zone.
#'
#' @param metricdat tibble; See Details
#' @param areadat tibble; See Details
#' @param class Character string containing the name of the field containing
#'   land cover classes by which `metricdat` and `areadat` should be joined;
#'   passed to [dplyr::full_join()].
#'
#' @return tibble
#' @seealso [sum_landcover()]
#' @export
#'
#' @examples
#' # See vignette

sum_metrics = function(metricdat, areadat) {

  dat_join = dplyr::full_join(areadat, metricdat) %>%
    dplyr::mutate(
      # retain mean value for Annual Wages for now, otherwise multiply by area
      # for total score
      SCORE_TOTAL = dplyr::if_else(METRIC == 'Annual Wages',
                                   SCORE_MEAN, area * SCORE_MEAN),
      # propagate error: multiplication by a constant
      SCORE_TOTAL_SE = dplyr::if_else(METRIC == 'Annual Wages',
                                      SCORE_SE, area * SCORE_SE),
      # remove "per ha" in the UNIT descriptions
      UNIT = dplyr::if_else(METRIC == 'Annual Wages',
                            UNIT, gsub(' per ha', '', UNIT))) %>%
    tidyr::replace_na(list(SCORE_TOTAL_SE = 0))
  # -->the only NA values are for land covers and metrics where value is
  # presumed zero and scores for climate change resilience to salinity

  bind_rows(
    # for all but annual wages, sum over all land cover classes:
    dat_join %>% dplyr::filter(METRIC != 'Annual Wages') %>%
      dplyr::group_by(
        across(
          any_of(
            c('scenario', 'ZONE', 'METRIC_CATEGORY', 'METRIC_SUBTYPE',
              'METRIC', 'UNIT')))) %>%
      dplyr::summarize(
        SCORE_TOTAL = sum(SCORE_TOTAL),
        SCORE_TOTAL_SE = sqrt(sum(SCORE_TOTAL_SE^2)),
        .groups = 'drop'
      ),
    # for annual wages: multiply the average wage per-landcover by the
    # proportion of the total ag landscape made up by that land cover
    dat_join %>% dplyr::filter(METRIC == 'Annual Wages' & SCORE_TOTAL > 0) %>%
      dplyr::group_by(
        across(
          any_of(
            c('scenario', 'ZONE', 'METRIC_CATEGORY', 'METRIC_SUBTYPE',
              'METRIC', 'UNIT')))) %>%
      dplyr::mutate(area_ag_total = sum(area),
                    area_prop = area / area_ag_total) %>%
      dplyr::summarize(SCORE_TOTAL = sum(SCORE_TOTAL * area_prop),
                SCORE_TOTAL_SE = sqrt(sum((SCORE_TOTAL_SE * area_prop)^2)),
                .groups = 'drop')
  )
}
