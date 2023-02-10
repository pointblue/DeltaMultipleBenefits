#' Update waterbird predictors: covertype
#'
#' Helper function for updating covertype predictors for the waterbird
#' distribution models.
#'
#' @details Classifies the `landscape` rasters according to the land cover
#'   classes that were originally surveyed, which are the only classes for which
#'   predictions should be generated from the waterbird distribution models.
#'   Generates file `covertype.tif` at location `pathout/SDM/scenario_name/`.
#'
#' @param landscape SpatRaster created by [terra::rast()]
#' @param key tibble, dataframe, or character string defining a filepath passed
#'   to [readr::read_csv()], used to interpret the raster values in `landscape`
#'   to land cover class names; see Details
#' @param SDM The name of intended species distribution model:
#'   `"waterbird_fall"`, or `"waterbird_win"`
#' @param mask Optional filepath to a raster that should be used to mask the
#'   output, e.g. a study area boundary
#' @param pathout,landscape_name Character strings defining the filepath
#'   (`pathout/SDM/landscape_name`) where output rasters should be written
#' @param overwrite Logical; passed to [terra::writeRaster()]; default `FALSE`
#'
#' @return Nothing; all files written to `pathout`
#' @seealso [update_pwater()]; [update_roosts()]
#' @export
#'
#' @examples
#' # See vignette

update_covertype = function(landscape, key, SDM, mask = NULL, pathout,
                              landscape_name, overwrite = FALSE) {

  if (is(key, 'character')) {
    if (grepl('csv', key)) {
      key = readr::read_csv(key, col_types = readr::cols())
    } else {
      stop('if key is a filepath, it should point to a CSV')
    }
  } else if (!(is(key, 'tbl') | is(key, 'data.frame'))) {
    stop('function expects "key" to be a character string, tibble, or data.frame')
  }

  if (SDM == 'waterbird_fall') {
    key_update = key %>%
      dplyr::mutate(
        covertype = dplyr::case_when(
          CODE_NAME == 'RICE' ~ 'Rice',
          CODE_NAME == 'PASTURE_OTHER' ~ 'Irrigated pasture',
          CODE_NAME == 'PASTURE_ALFALFA' ~ 'Alfalfa',
          CODE_NAME %in%
            c('WETLAND_MANAGED_PERENNIAL', 'WETLAND_MANAGED_SEASONAL') ~ 'Wetland',
          TRUE ~ NA_character_),
        covertype_code = dplyr::case_when(
          covertype == 'Alfalfa' ~ 1,
          covertype == 'Irrigated pasture' ~ 2,
          covertype == 'Rice' ~ 3,
          covertype == 'Wetland' ~ 4)) %>%
      dplyr::select(.data$CODE_NAME, .data$CODE_BASELINE,
                    .data$covertype, .data$covertype_code) %>%
      tidyr::drop_na()

  } else if (SDM == 'waterbird_win') {
    key_update = key %>%
      dplyr::mutate(
        covertype = dplyr::case_when(
          CODE_NAME == 'PASTURE_ALFALFA' ~ 'Alfalfa',
          CODE_NAME == 'FIELD_CORN' ~ 'Corn',
          CODE_NAME == 'PASTURE_OTHER' ~ 'Irrigated pasture',
          CODE_NAME == 'RICE' ~ 'Rice',
          CODE_NAME %in%
            c('WETLAND_MANAGED_PERENNIAL', 'WETLAND_MANAGED_SEASONAL') ~ 'Wetland',
          CODE_NAME == 'GRAIN&HAY_WHEAT' ~ 'Winter wheat',
          TRUE ~ NA_character_),
        covertype_code = dplyr::case_when(
          covertype == 'Alfalfa' ~ 1,
          covertype == 'Corn' ~ 2,
          covertype == 'Irrigated pasture' ~ 3,
          covertype == 'Rice' ~ 4,
          covertype == 'Wetland' ~ 5,
          covertype == 'Winter wheat' ~ 6)) %>%
      dplyr::select(.data$CODE_NAME, .data$CODE_BASELINE,
                    .data$covertype, .data$covertype_code) %>%
      tidyr::drop_na()
  }

  if (!is.null(mask)) {
    if (is(mask, 'character')) {
      mask = terra::rast(mask)
    } else if (!is(mask, 'SpatRaster')) {
      stop('function expects "mask" to be either a character string or a SpatRaster')
    }
    landscape = terra::mask(landscape, terra::rast(mask))
  }

  covertype = terra::classify(
    landscape,
    rcl = key_update %>%
      dplyr::select(from = .data$CODE_BASELINE,
                    to = .data$covertype_code) %>%
      as.matrix(),
    others = NA)
  levels(covertype) = key_update %>%
    dplyr::select(.data$covertype_code, .data$covertype) %>%
    dplyr::distinct() %>%
    tidyr::drop_na() %>%
    dplyr::arrange(.data$covertype_code) %>%
    as.data.frame()

  create_directory(file.path(pathout, SDM, landscape_name))
  terra::writeRaster(covertype,
                     file.path(pathout, SDM, landscape_name, 'covertype.tif'),
                     overwrite = overwrite)
}
