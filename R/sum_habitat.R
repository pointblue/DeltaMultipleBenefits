#' Summarize total habitat scores
#'
#' Calculate the sum of the predicted presence/absence or probability of
#' presence from species distribution models corresponding to a set of landscape
#' rasters.
#'
#' @details This function will calculate the sum of all pixel values in each
#'   raster found in `pathin` and any subdirectories. The file structure within
#'   `pathin` is used to infer the name of the corresponding `SDM` and
#'   `landscape_name`. This function is designed to efficiently
#'   process multiple rasters located in the `pathin` directory at once.
#'
#'   If provided, `keypath` should be a CSV containing the fields `spp` and
#'   `label`, used for converting the filenames of the rasters within `pathin`
#'   to a more readable label, which will be renamed `METRIC` in the final
#'   output.
#'
#' @param pathin Character string defining the filepath to the highest-level
#'   directory containing the predicted presence/absence or probability of
#'   presence from species distribution models, such as those created from
#'   running [fit_SDM()] or [transform_SDM()]
#' @param zonepath Optional character string defining the filepath to a raster
#'   encoding zones within which pixel values should be summarized
#' @param subtype Character string appended to the field METRIC_SUBTYPE, such as
#'   for distinguishing probability of presence from presence/absence
#' @param keypath Optional filepath passed to [readr::read_csv()] translating
#'   the individual species names as encoded in the file names in pathin to
#'   METRIC names in the output table
#'
#' @return tibble
#' @seealso [fit_SDM()], [transform_SDM()]
#' @export
#'
#' @examples
#' # See vignette

sum_habitat = function(pathin, zonepath = NULL, subtype = 'distributions',
                       keypath = NULL) {
  fl = list.files(pathin, '.tif$', recursive = TRUE, full.names = TRUE) %>%
    set_names()

  if (is.null(zones)) {
    # sum total
    res = purrr::map_df(
      fl,
      ~terra::rast(.x) %>% terra::values() %>% sum(na.rm = TRUE) %>%
        dplyr::as_tibble(),
      .id = 'pathin')
  } else {
    # zonal total
    res = purrr::map_df(
      fl,
      ~terra::rast(.x) %>%
        terra::zonal(terra::rast(zonepath), 'sum', na.rm = TRUE) %>%
        setNames(c('ZONE', 'value')),
      .id = 'pathin'
    )
  }

  res = res %>%
    dplyr::mutate(pathin = gsub(!!pathin, '', pathin),
           pathin = gsub('^\\/|.tif$', '', pathin)) %>%
    dplyr::separate(pathin, sep = '/', into = c('SDM', 'scenario', 'spp')) %>%
    dplyr::mutate(
      METRIC_CATEGORY = 'Biodiversity Support',
      METRIC_SUBTYPE = dplyr::case_when(
        SDM == 'riparian' ~ paste('Riparian landbird', subtype),
        SDM %in% c('waterbird_fall', 'waterbird_win') ~
          paste('Waterbird', subtype)),
      SCORE_TOTAL = value / .09, # convert to ha
      UNIT = 'ha')

  if (!is.null(keypath)) {
    res = dplyr::left_join(
      readr::read_csv(keypath, col_types = readr::cols()) %>%
        dplyr::select(spp, METRIC = label), by = 'spp')
  } else {
    res = dplyr::rename(res, METRIC = spp)
  }

  res %>%
    dplyr::mutate(
      METRIC = dplyr::case_when(
        SDM == 'waterbird_fall' ~ paste0(METRIC, ' (fall)'),
        SDM == 'waterbird_win' ~ paste0(METRIC, ' (winter)'),
        TRUE ~ METRIC)) %>%
    dplyr::select(scenario, any_of('ZONE'), METRIC_CATEGORY, METRIC_SUBTYPE,
                  METRIC, UNIT, SCORE_TOTAL)
}
