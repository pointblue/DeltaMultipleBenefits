#' Summarize total habitat scores
#'
#' Calculate the sum of the predicted presence/absence or probability of
#' presence from species distribution models corresponding to a set of landscape
#' rasters.
#'
#' @details This function will calculate the sum of all pixel values in each
#'   raster found in `pathin` and any subdirectories. The file structure within
#'   `pathin` is used to infer the name of the corresponding `SDM` and
#'   `landscape_name`. This function is designed to efficiently process multiple
#'   rasters located in the `pathin` directory at once.
#'
#'   If provided, `keypath` should be a CSV containing the fields `spp` and
#'   `label`, used for converting the filenames of the rasters within `pathin`
#'   to a more readable label, which will be renamed `METRIC` in the final
#'   output.
#'
#'   If provided, the sum of all pixel values will be multiplied by `scale`. For
#'   example, to rescale the total in terms of the total area, enter the area of
#'   each pixel.
#'
#' @param pathin Character string defining the filepath to the highest-level
#'   directory containing the predicted presence/absence or probability of
#'   presence from species distribution models, such as those created from
#'   running [fit_SDM()] or [transform_SDM()]
#' @param zonepath Optional character string defining the filepath to a raster
#'   encoding zones within which pixel values should be summarized
#' @param subtype Character string appended to the field METRIC_SUBTYPE, such as
#'   for distinguishing probability of presence from presence/absence
#' @param rollup Logical; If `TRUE` (default), summarize total habitat across
#'   all species/groups by set of SDMs
#' @param keypath Optional filepath passed to [readr::read_csv()] translating
#'   the individual species names as encoded in the file names in pathin to
#'   METRIC names in the output table
#' @param scale Optional value by which to scale the results; see Details
#'
#' @return tibble
#' @seealso [fit_SDM()], [transform_SDM()]
#' @export
#'
#' @examples
#' # See vignette

sum_habitat = function(pathin, zonepath = NULL, subtype = 'distributions',
                       rollup = TRUE, keypath = NULL, scale = NULL) {
  fl = list.files(pathin, '.tif$', recursive = TRUE, full.names = TRUE) %>%
    rlang::set_names()

  if (is.null(zonepath)) {
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
        dplyr::as_tibble() %>%
        rlang::set_names(c('ZONE', 'value')),
      .id = 'pathin'
    )
  }

  res = res %>%
    dplyr::mutate(pathin = gsub(!!pathin, '', pathin),
           pathin = gsub('^\\/|.tif$', '', pathin)) %>%
    tidyr::separate(pathin, sep = '/', into = c('SDM', 'scenario', 'spp'))

  if (rollup) {
    # first find max value across all rasters for a scenario and SDM, then sum over landscape

    totals = purrr::pmap_df(
      res %>% dplyr::select(SDM, scenario) %>% dplyr::distinct(),
      function(SDM, scenario) {
        combined = list.files(file.path(pathin, SDM, scenario),
                              '.tif$', full.names = TRUE) %>%
          terra::rast() %>% max(na.rm = TRUE)
        if (is.null(zonepath)) {
          terra::values(combined) %>% sum(na.rm = TRUE) %>%
            dplyr::as_tibble()
        } else {
          terra::zonal(combined, terra::rast(zonepath), 'sum', na.rm = TRUE) %>%
            dplyr::as_tibble() %>%
            rlang::set_names(c('ZONE', 'value'))
        }
      }) %>%
      dplyr::bind_cols(res %>%
                         dplyr::select(SDM, dplyr::any_of('ZONE'), scenario) %>%
                         dplyr::distinct() %>% select(-any_of('ZONE'))) %>%
      mutate(spp = 'TOTAL')

      res = dplyr::bind_rows(res, totals)

  }

  if (!is.null(keypath)) {
    res = res %>%
      dplyr::left_join(
        readr::read_csv(keypath, col_types = readr::cols()) %>%
          dplyr::select(spp, METRIC = label),
        by = 'spp')
  } else {
    res = dplyr::rename(res, METRIC = spp)
  }

  res = res %>%
    dplyr::mutate(
      METRIC_CATEGORY = 'Biodiversity Support',
      METRIC_SUBTYPE = dplyr::case_when(
        SDM == 'riparian' ~ paste('Riparian landbird', subtype),
        SDM %in% c('waterbird_fall', 'waterbird_win') ~
          paste('Waterbird', subtype)),
      METRIC = dplyr::case_when(
        SDM == 'waterbird_fall' ~ paste0(METRIC, ' (fall)'),
        SDM == 'waterbird_win' ~ paste0(METRIC, ' (winter)'),
        TRUE ~ METRIC),
      SCORE_TOTAL = value) %>%
    dplyr::select(scenario, any_of('ZONE'), METRIC_CATEGORY, METRIC_SUBTYPE,
                  METRIC, SCORE_TOTAL)

  if (!is.null(scale)) {
    res = res %>%
      mutate(SCORE_TOTAL = SCORE_TOTAL * scale)
  }

  return(res)

}
