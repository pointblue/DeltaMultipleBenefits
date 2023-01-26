#' Summarize total habitat scores
#'
#' Calculate the sum of the predicted presence/absence or probability of
#' presence from species distribution models corresponding to a set of landscape
#' rasters.
#'
#' @details By default, this function will calculate the sum of all pixel values
#'   in each raster found in `pathin` and any subdirectories, to efficiently
#'   process multiple rasters located in the `pathin` directory at once. The
#'   file structure within `pathin` is used to infer the name of the
#'   corresponding `SDM` and `landscape_name`. However, `SDM` or both `SDM` and
#'   `landscape_name` can be optionally specified to only process a subset of
#'   these.
#'
#'   If provided, `key` should refer to a tibble, dataframe, or filepath to a
#'   CSV containing the fields `spp` and `label`, used for converting the
#'   filenames of the rasters within `pathin` to a more readable label, which
#'   will be renamed `METRIC` in the final output.
#'
#'   If provided, the sum of all pixel values will be multiplied by `scale`. For
#'   example, to rescale the total in terms of the total area, enter the area of
#'   each pixel.
#'
#' @param pathin Character string defining the filepath to the highest-level
#'   directory containing the predicted presence/absence or probability of
#'   presence from species distribution models, such as those created from
#'   running [fit_SDM()] or [transform_SDM()]
#' @param SDM,landscape_name Optional character strings defining the
#'   subdirectories within `pathin` for which habitat should be summarized; see
#'   Details; SDM must be one of `"riparian"`, `"waterbird_fall"`, or
#'   `"waterbird_win"`.
#' @param zones Optional `SpatRaster` or character string giving the filepath to
#'   a raster encoding zones within which pixel values should be summarized
#' @param subtype Optional character string appended to the field
#'   METRIC_SUBTYPE, such as for distinguishing probability of presence from
#'   presence/absence
#' @param rollup Logical; If `TRUE` (default), summarize total habitat across
#'   all species/groups by set of SDMs
#' @param key Optional tibble, dataframe, or character string defining the
#'   filepath passed to [readr::read_csv()], used to translate the individual
#'   species names as encoded in the file names in pathin to readable METRIC
#'   names in the output table; see Details
#' @param scale Optional value by which to scale the results; see Details
#'
#' @return tibble
#' @seealso [fit_SDM()], [transform_SDM()]
#' @importFrom methods is
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom readr read_csv
#' @export
#'
#' @examples
#' # See vignette

sum_habitat = function(pathin, SDM = NULL, landscape_name = NULL,
                       zones = NULL, subtype = NULL,
                       rollup = TRUE, key = NULL, scale = NULL) {

  if (is.null(SDM)) {
    # assume landscape_name also NULL (and ignore if not)
    # recursively pull in anything in pathin
    fl = list.files(pathin, '.tif$', recursive = TRUE, full.names = TRUE) %>%
      rlang::set_names()
  } else {
    if (is.null(landscape_name)) {
      #recursively pull in anything in pathin/SDM
      fl = list.files(file.path(pathin, SDM), '.tif$', recursive = TRUE,
                      full.names = TRUE) %>%
        rlang::set_names()
    } else {
      fl = list.files(file.path(pathin, SDM, landscape_name), '.tif$',
                      full.names = TRUE) %>%
        rlang::set_names()
    }
  }

  if (is.null(zones)) {
    # sum total
    res = purrr::map_df(
      fl,
      ~terra::rast(.x) %>% terra::values() %>% sum(na.rm = TRUE) %>%
        dplyr::as_tibble(),
      .id = 'pathin')
  } else {

    if (is(zones, 'character')) {
      zones = terra::rast(zones)
    } else if (!is(zones, 'SpatRaster')) {
      stop('function expects "zones" to be either a character string or a SpatRaster')
    }

    # zonal total
    res = purrr::map_df(
      fl,
      ~terra::rast(.x) %>%
        terra::zonal(zones, 'sum', na.rm = TRUE) %>%
        dplyr::as_tibble() %>%
        rlang::set_names(c('ZONE', 'value')),
      .id = 'pathin'
    )
  }

  res = res %>%
    dplyr::mutate(pathin = gsub(!!pathin, '', pathin),
           pathin = gsub('^\\/|.tif$', '', pathin)) %>%
    tidyr::separate(pathin, sep = '/', into = c('SDM', 'scenario', 'spp'),
                    fill = 'left')

  if (rollup) {
    # first find max value across all rasters for a scenario and SDM, then sum over landscape

    totals = purrr::pmap_df(
      res %>% dplyr::select(.data$SDM, .data$scenario) %>% dplyr::distinct(),
      function(SDM, scenario) {
        combined = list.files(file.path(pathin, SDM, scenario),
                              '.tif$', full.names = TRUE) %>%
          terra::rast() %>% max(na.rm = TRUE)
        if (is.null(zones)) {
          terra::values(combined) %>% sum(na.rm = TRUE) %>%
            dplyr::as_tibble()
        } else {
          terra::zonal(combined, zones, 'sum', na.rm = TRUE) %>%
            dplyr::as_tibble() %>%
            rlang::set_names(c('ZONE', 'value'))
        }
      }) %>%
      dplyr::bind_cols(res %>%
                         dplyr::select(.data$SDM, dplyr::any_of('ZONE'),
                                       .data$scenario) %>%
                         dplyr::distinct() %>%
                         dplyr::select(-dplyr::any_of('ZONE'))) %>%
      dplyr::mutate(spp = 'TOTAL')

      res = dplyr::bind_rows(res, totals)

  }

  if (!is.null(key)) {
    if (is(key, 'character')) {
      key = readr::read_csv(key, col_types = readr::cols())
    } else if (!(is(key, 'tbl') | is(key, 'data.frame'))) {
      stop('function expects "key" to be a character string, tibble, or data.frame')
    }

    res = res %>%
      dplyr::left_join(key %>% dplyr::select(.data$spp, METRIC = .data$label),
                       by = 'spp')
  } else {
    res = dplyr::rename(res, METRIC = .data$spp)
  }

  res = res %>%
    dplyr::mutate(
      METRIC_CATEGORY = 'Biodiversity Support',
      METRIC_SUBTYPE = dplyr::case_when(
        SDM == 'riparian' ~ 'Riparian landbird',
        SDM %in% c('waterbird_fall', 'waterbird_win') ~ 'Waterbird'),
      METRIC = dplyr::case_when(
        SDM == 'waterbird_fall' ~ paste0(.data$METRIC, ' (fall)'),
        SDM == 'waterbird_win' ~ paste0(.data$METRIC, ' (winter)'),
        TRUE ~ METRIC),
      SCORE_TOTAL = .data$value) %>%
    dplyr::select(.data$scenario, dplyr::any_of('ZONE'), .data$METRIC_CATEGORY,
                  .data$METRIC_SUBTYPE, .data$METRIC, .data$SCORE_TOTAL)

  if (!is.null(subtype)) {
    res = res %>%
      dplyr::mutate(METRIC_SUBTYPE = paste(.data$METRIC_SUBTYPE, subtype))
  }

  if (!is.null(scale)) {
    res = res %>%
      dplyr::mutate(SCORE_TOTAL = .data$SCORE_TOTAL * scale)
  }

  return(res)

}
