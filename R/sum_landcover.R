#' Summarize total area of land cover classes
#'
#' Summarize the area or number of pixels of each land cover class from a set of
#' landscape rasters
#'
#' @details This function will summarize the frequency of pixels of each land
#'   cover class in each landscape raster, either provided as a `SpatRaster`
#'   created by [terra::rast()] with one or more layers, or as a filepath to a
#'   directory containing one or more `tif` raster files to be summarized. This
#'   function is generally agnostic as to the names of the rasters and the land
#'   cover classes it expects, but it does expect the rasters to have unique
#'   names and to have land cover class levels defined (i.e., not just contain
#'   numeric codes; see [terra::levels()]). However, if `rollup = TRUE` (the
#'   default), riparian and managed wetland subclasses are expected to have
#'   names that begin with `'RIPARIAN_'` or `'WETLAND_MANAGED_'`. These
#'   subclasses will be summarized into a total area of `'RIPARIAN'` and
#'   `'WETLAND_MANAGED'` land cover classes.
#'
#'   This function relies on [terra::freq()] to summarize the total number of
#'   cells of each value within each raster and optionally each zone provided,
#'   which are then multiplied by `pixel_area` as an estimate of the total area
#'   of each land cover class in each landscape. The value for `pixel_area` can
#'   be provided in any units you prefer, but if this function is intended to
#'   produce `areadat` for use with [sum_metrics()], we recommend using hectares
#'   to ensure alignment with the units in which metrics were estimated.
#'
#'   By default, the total area of each unique land cover class will be
#'   summarized for the entire raster provided, but optionally, the area can be
#'   subset by providing a `mask` and/or `zones`. A `mask` should be a single
#'   `SpatRaster` or filepath to a `tif` raster file that will be passed to
#'   [terra::mask()] to exclude any areas with `NA` values. In addition, `zones`
#'   should be a single `SpatRaster` or filepath to a `tif` raster file that
#'   will be passed to [terra::zonal()]. Values in `zones` should define two or
#'   more unique regions within which the area of each land cover class should
#'   be summarized, and the names of each region should be defined (see
#'   [terra::levels()]).
#'
#' @param rasters Either a `SpatRaster` with one or more layers, or a character
#'   string giving the filepath to a directory containing one or more raster
#'   landscapes (e.g. scenarios) to be summarized.
#' @param mask Optional `SpatRaster` or character string giving the filepath to
#'   a raster that should be used to mask the output, e.g. a study area boundary
#' @param zones Optional `SpatRaster` or character string giving the filepath to
#'   a raster encoding zones within which pixel values should be summarized
#' @param pixel_area Numeric value representing the area of each pixel; hectares
#'   preferred but any units may be provided; default is 1, resulting in a total
#'   count
#' @param rollup Logical; whether or not riparian and managed wetland subclasses
#'   should be rolled up into a total area of riparian and managed wetlands,
#'   respectively. See Details.
#'
#' @return tibble
#' @seealso [sum_habitat()], [sum_metrics()]
#' @importFrom methods is
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' # See vignette

sum_landcover = function(rasters, mask = NULL, zones = NULL, pixel_area = 1,
                         rollup = TRUE) {

  if (is(rasters, 'character')) {
    # treat as filepath, and bring in file names:
    fl = list.files(rasters, '.tif$', full.names = TRUE) %>%
      rlang::set_names(gsub('.tif', '', list.files(rasters, '.tif$')))
    rasters = purrr::map(fl, ~terra::rast(.x)) %>% terra::rast()
  } else if (!is(rasters, 'SpatRaster')) {
    stop('function expects "rasters" to be either a character string or a SpatRaster')
  }

  if (!is.null(mask)) {
    if (is(mask, 'character')) {
      mask = terra::rast(mask)
    } else if (!is(mask, 'SpatRaster')) {
      stop('function expects "mask" to be either a character string or a SpatRaster')
    }
    rasters = terra::mask(rasters, mask)
  }

  if (is.null(zones)) {
    # total area of each unique value for each layer in rasters
    res = terra::freq(rasters, bylayer = TRUE, usenames = TRUE) %>%
      as.data.frame() %>%
      dplyr::mutate(area = .data$count * pixel_area) %>%
      dplyr::select(scenario = .data$layer, CODE_NAME = .data$label, .data$area) %>%
      dplyr::arrange(.data$scenario, .data$CODE_NAME)

  } else {
    # summarize area of each value within zones for each layer

    if (is(zones, 'character')) {
      zones = terra::rast(zones)
    } else if (!is(zones, 'SpatRaster')) {
      stop('function expects "mask" to be either a character string or a SpatRaster')
    }
    znames = terra::freq(zones)$label
    zseg = terra::segregate(zones, other = NA)

    res = purrr::map_df(
      terra::as.list(rasters) %>% rlang::set_names(names(rasters)),
      ~terra::zonal(zseg, .x, 'sum', na.rm = TRUE) %>%
        rlang::set_names(c('label', znames)),
      .id = 'layer') %>%
      tidyr::pivot_longer(dplyr::all_of(znames),
                          names_to = 'ZONE', values_to = 'count') %>%
      dplyr::mutate(area = .data$count * pixel_area) %>%
      dplyr::select(scenario = .data$layer, .data$ZONE, CODE_NAME = .data$label,
                    .data$area) %>%
      dplyr::arrange(.data$scenario, .data$ZONE, .data$CODE_NAME)
  }

  if(rollup) {
    # add roll-up of riparian & managed wetland subtypes
    res = dplyr::bind_rows(
      res,
      res %>% dplyr::filter(grepl('RIPARIAN_|WETLAND_MANAGED', .data$CODE_NAME)) %>%
        dplyr::mutate(
          CODE_NAME = dplyr::case_when(
            grepl('RIPARIAN_', .data$CODE_NAME) ~ 'RIPARIAN',
            grepl('WETLAND_MANAGED', .data$CODE_NAME) ~ 'WETLAND_MANAGED',
            TRUE ~ .data$CODE_NAME)) %>%
        dplyr::group_by(
          dplyr::across(dplyr::any_of(c('scenario', 'ZONE', 'CODE_NAME')))) %>%
        # dplyr::group_by(scenario, CODE_NAME) %>%
        dplyr::summarize(area = sum(.data$area), .groups = 'drop'))
  }

  return(res)
}
