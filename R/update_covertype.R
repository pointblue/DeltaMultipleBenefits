#' Update waterbird predictors: covertype
#'
#' Helper function for updating covertype predictors for the waterbird
#' distribution models.
#'
#' @details Classifies the `landscape` rasters according to the land cover
#'   classes that were originally surveyed, which are the only classes for which
#'   predictions should be generated from the waterbird distribution models.
#'   Generates file: `covertype.tif` at location `pathout/scenario_name/`.
#'
#' @param landscape SpatRaster created by [terra::rast()]
#' @param SDM The name of intended species distribution model:
#'   `"waterbird_fall"`, or `"waterbird_win"`
#' @param maskpath Optional filepath to a raster that should be used to mask the
#'   output, e.g. a study area boundary
#' @param pathout Character string; Filepath to directory where output rasters
#'   should be written; passed to [terra::writeRaster()]
#' @param scenario_name Character string; Name of the landscape scenario being
#'   evaluated, corresponding to the directory in `pathout` where results will
#'   be written.
#' @param overwrite Logical; passed to [terra::writeRaster()]; default `FALSE`
#'
#' @return Nothing; all files written to `pathout`
#' @seealso [update_pwater()]; [update_roosts()]
#' @export
#'
#' @examples
#' # See vignette

update_covertype = function(landscape, SDM, maskpath = NULL, pathout,
                              scenario_name, overwrite = FALSE) {

  if (SDM == 'waterbird_fall') {
    key = terra::freq(landscape) %>%
      dplyr::mutate(
        covertype = dplyr::case_when(
          label == 'RICE' ~ 'Rice',
          label == 'PASTURE_OTHER' ~ 'Irrigated pasture',
          label == 'PASTURE_ALFALFA' ~ 'Alfalfa',
          label %in%
            c('WETLAND_MANAGED_PERENNIAL', 'WETLAND_MANAGED_SEASONAL') ~ 'Wetland',
          TRUE ~ NA_character_),
        covertype_code = dplyr::case_when(
          covertype == 'Alfalfa' ~ 1,
          covertype == 'Irrigated pasture' ~ 2,
          covertype == 'Rice' ~ 3,
          covertype == 'Wetland' ~ 4)
      )

  } else if (SDM == 'waterbird_win') {
    key = terra::freq(landscape) %>%
      dplyr::mutate(
        covertype = dplyr::case_when(
          label == 'PASTURE_ALFALFA' ~ 'Alfalfa',
          label == 'FIELD_CORN' ~ 'Corn',
          label == 'PASTURE_OTHER' ~ 'Irrigated pasture',
          label == 'RICE' ~ 'Rice',
          label %in%
            c('WETLAND_MANAGED_PERENNIAL', 'WETLAND_MANAGED_SEASONAL') ~ 'Wetland',
          label == 'GRAIN&HAY_WHEAT' ~ 'Winter wheat',
          TRUE ~ NA_character_),
        covertype_code = dplyr::case_when(
          covertype == 'Alfalfa' ~ 1,
          covertype == 'Corn' ~ 2,
          covertype == 'Irrigated pasture' ~ 3,
          covertype == 'Rice' ~ 4,
          covertype == 'Wetland' ~ 5,
          covertype == 'Winter wheat' ~ 6)
      )
  }

  if (!is.null(maskpath)) {
    landscape = terra::mask(landscape, terra::rast(maskpath))
  }

  covertype = terra::classify(landscape,
                       rcl = key %>%
                         dplyr::select(from = value, to = covertype_code),
                       othersNA = TRUE)
  levels(covertype) = key %>% dplyr::select(covertype_code, covertype) %>%
    dplyr::distinct() %>%
    tidyr::drop_na() %>%
    dplyr::arrange(covertype_code) %>%
    as.data.frame()

  create_directory(file.path(pathout, scenario_name))
  terra::writeRaster(covertype,
                     file.path(pathout, scenario_name, 'covertype.tif'),
                     overwrite = overwrite)
}
