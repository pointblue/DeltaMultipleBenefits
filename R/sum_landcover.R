#' Summarize total area of land cover classes
#'
#' Summarize the area or number of pixels of each land cover class from a set of
#' landscape rasters
#'
#' @details This function will summarize the frequency of pixels of each land
#'   cover class in each landscape raster provided in `pathin`, optionally
#'   masked by a raster at `maskpath`, and/or optionally summarized by zones
#'   encoded in a raster at `zonepath`. The resulting number of pixels is
#'   multiplied by `pixel_area` as an estimate of the total area of each land
#'   cover class in each landscape. This function is designed to efficiently
#'   process several landscapes located in the `pathin` directory at once.
#'
#'   This function is generally agnostic as to the names of the land cover
#'   classes it expects, but it does expect the landscapes to have levels
#'   defined (see [terra::levels()]) and not just contained numeric codes. One
#'   exception is if `rollup = TRUE`, in which case it will expect to find
#'   riparian and managed wetland subclasses with names that begin with
#'   `'RIPARIAN_'` or `'WETLAND_MANAGED_'`. These subclasses will be summarized
#'   into a total area of `'RIPARIAN'` and `'WETLAND_MANAGED'` land cover
#'   classes.
#'
#'   `pixel_area` can be provided in any units you prefer, but if this function
#'   is intended to produce `areadat` for use with [sum_metrics()], take care
#'   that these units align with the UNITs by which `metricdat` are defined
#'
#' @param pathin Character string defining the filepath to a directory
#'   containing landscapes (e.g. scenarios) to be summarized.
#' @param maskpath Optional filepath to a raster that should be used to mask the
#'   output, e.g. a study area boundary
#' @param pixel_area Numeric value representing the area of each pixel, in any
#'   units; will be multiplied by the number of pixels of each land cover class
#' @param zonepath Optional character string defining the filepath to a raster
#'   encoding zones within which pixel values should be summarized
#' @param rollup Logical; whether or not riparian and managed wetland subclasses
#'   should be rolled up into a total area of riparian and managed wetlands,
#'   respectively.
#'
#' @return tibble
#' @seealso [sum_habitat()], [sum_metrics()]
#' @export
#'
#' @examples
#' # See vignette

sum_landcover = function(pathin, maskpath, pixel_area = 1, zonepath = NULL,
                         rollup = TRUE) {
  fl = list.files(pathin, '.tif$', full.names = TRUE) %>% set_names()

  if (is.null(zonepath)) {

    if (is.null(maskpath)) {

      res = purrr::map_df(
        fl,
        ~terra::rast(.x) %>% terra::freq() %>%
          dplyr::mutate(area = count * pixel_area),
        .id = 'scenario')

    } else {

      res = purrr::map_df(
        fl,
        ~terra::rast(.x) %>% terra::mask(terra::rast(maskpath)) %>%
          terra::freq() %>% dplyr::mutate(area = count * pixel_area),
        .id = 'scenario')

    }

    res = res %>%
      dplyr::mutate(scenario = gsub(!!pathin, '', scenario),
                    scenario = gsub('^\\/|.tif$', '', scenario)) %>%
      dplyr::select(scenario, CODE_NAME = label, area) %>%
      dplyr::as_tibble()

    if(rollup) {
      # add roll-up of riparian & managed wetland subtypes
      res = dplyr::bind_rows(
        res,
        res %>% dplyr::filter(grepl('RIPARIAN_|WETLAND_MANAGED', CODE_NAME)) %>%
          dplyr::mutate(
            CODE_NAME = dplyr::case_when(
              grepl('RIPARIAN_', CODE_NAME) ~ 'RIPARIAN',
              grepl('WETLAND_MANAGED', CODE_NAME) ~ 'WETLAND_MANAGED',
              TRUE ~ CODE_NAME)) %>%
          dplyr::group_by(scenario, CODE_NAME) %>%
          dplyr::summarize(area = sum(area), .groups = 'drop'))
    }

  } else {
    zones = terra::rast(zonepath)
    znames = terra::levels(zones)[[1]]
    zseg = terra::segregate(zones, other = NA)

    if (is.null(maskpath)) {

      res = purrr::map_df(
        fl,
        ~terra::zonal(zseg, terra::rast(.x), 'sum', na.rm = TRUE) %>%
          rlang::set_names(c('CODE_NAME', znames)),
        .id = 'scenario')
    } else {
      res = purrr::map_df(
        fl,
        ~terra::zonal(zseg, terra::mask(terra::rast(.x), terra::rast(maskpath)),
                      'sum', na.rm = TRUE) %>%
          rlang::set_names(c('CODE_NAME', znames)),
        .id = 'scenario')
    }

    res = res %>%
      dplyr::pivot_longer(znames, names_to = 'ZONE', values_to = 'count') %>%
      dplyr::mutate(area = count * pixel_area) %>%
      dplyr::mutate(scenario = gsub(!!pathin, '', scenario),
                    scenario = gsub('^\\/|.tif$', '', scenario)) %>%
      dplyr::select(scenario, ZONE, CODE_NAME, area) %>%
      dplyr::as_tibble()

    if(rollup) {
      # add roll-up of riparian & managed wetland subtypes
      res = dplyr::bind_rows(
        res,
        res %>% dplyr::filter(grepl('RIPARIAN_|WETLAND_MANAGED', CODE_NAME)) %>%
          dplyr::mutate(
            CODE_NAME = dplyr::case_when(
              grepl('RIPARIAN_', CODE_NAME) ~ 'RIPARIAN',
              grepl('WETLAND_MANAGED', CODE_NAME) ~ 'WETLAND_MANAGED',
              TRUE ~ CODE_NAME)) %>%
          dplyr::group_by(scenario, ZONE, CODE_NAME) %>%
          dplyr::summarize(area = sum(area), .groups = 'drop'))
    }

  }
  return(res)
}
