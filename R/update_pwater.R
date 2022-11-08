#' Update waterbird predictors: pwater & pfld
#'
#' Helper function for updating pwater and pfld predictors for the waterbird
#' distribution models.
#'
#' @details Uses surface water data in `waterdatpath` to create updated pwater
#'   predictors and inform pfld predictors for use with water bird models.
#'
#'   Pwater represents the direct probability of local open surface water at
#'   each pixel. If only `landscape` is provided, the water data from
#'   `waterdatpath` is directly used. If `scenario_landscape` is also provided,
#'   `landscape` is assumed to represent a baseline condition against which
#'   `scenario_landscape` should be compared. In this case, new cell-specific
#'   probabilities of open surface water for the `scenario_landscape` are
#'   generated for cells that have changed land cover classes, based on the mean
#'   probability of open surface water for that land cover class in the
#'   baseline. If `floor = TRUE`, new probabilities of open water will be
#'   assigned only if they are higher than the baseline values. If `maskpath` is
#'   not NULL, will also optionally mask by the provided study area. Generates
#'   file: `pwater.tif` at location `pathout[2]/scenario_name`, where
#'   `pathout[2]` is intended to be the ultimate directory holding all
#'   predictors for waterbird distribution models.
#'
#'   This same pwater.tif file is also necessary as an input for using
#'   [python_focal_prep()] and then [python_focal()] to generate the pfld focal
#'   stats, which represent the proportion of the area of each land cover class
#'   within a given distance that has open surface water. However, this version
#'   of pwater.tif file must be unmasked by any study area boundary to allow
#'   flooding from the surrounding buffer to be incorporated in the pfld focal
#'   stats. Thus, this function also generates a separate file: `pwater.tif` at
#'   location `pathout[1]/scenario_name`, where `pathout[1]` is intended to
#'   contain intermediate files for use in [python_focal_prep()] with `mask =
#'   'pwater.tif'`. The argument `pathout` must therefore provide two
#'   directories to provide a location for each version of `pwater.tif`. For
#'   example: `pathout = c('GIS/landscape_rasters/pwater', GIS/landscape_rasters/predictors_waterbird_fall')`.
#'
#' @param landscape SpatRaster created by [terra::rast()]
#' @param scenario_landscape Optional secondary SpatRaster created by
#'   [terra::rast()] to compare against baseline_landscape; see Details.
#' @param pathout Vector of two character strings providing filepaths where
#'   output rasters should be written; see Details
#' @param scenario_name Character string; Name of the landscape scenario being
#'   evaluated, corresponding to the directory in `pathout` where results will
#'   be written.
#' @param overwrite Logical; passed to [terra::writeRaster()]; default `FALSE`
#' @param waterdatpath Filepath to a raster representing the probability of open
#'   water (pwater) in each cell, specific to the time frames for each waterbird
#'   SDM
#' @param maskpath Optional filepath to a raster that should be used to mask the
#'   output, e.g. a study area boundary
#' @param floor Logical; if `TRUE`, don't allow new values of pwater to be lower
#'   than baseline values
#'
#' @return Nothing; all files written to `pathout`
#' @seealso [update_covertype()], [update_roosts()]
#' @export
#'
#' @examples
#' # See vignette


update_pwater = function(landscape, scenario_landscape = NULL,
                         pathout, scenario_name = NULL, overwrite = FALSE,
                         waterdatpath, floor = TRUE, maskpath = NULL) {

  pwater = terra::rast(waterdatpath)

  if (!is.null(scenario_landscape)) { # generate new pwater values for scenario

    # calculate mean baseline pwater by land cover class (detailed) - including
    # within the 10km buffer
    mwater = terra::zonal(pwater, landscape, fun = mean, na.rm = TRUE) %>%
      setNames(c('label', 'pwater')) %>%
      tidyr::drop_na() %>%
      dplyr::left_join(terra::freq(landscape), by = 'label')

    # assign mean baseline pwater values to changed pixels in each scenario
    changes = terra::diff(c(scenario_landscape, landscape)) %>%
      terra::subst(from = 0, to = NA) %>% #no change = NA
      terra::classify(rcl = matrix(c(-Inf, Inf, 1), nrow = 1)) # all others = 1

    pwater_new = scenario_landscape %>% terra::mask(changes) %>%
      terra::classify(rcl = mwater %>% dplyr::select(value, pwater) %>%
                        as.matrix(), othersNA = TRUE)

    if (floor) {
      # current mean pwater is the "floor"; only allow pwater to increase for
      # changed pixels (e.g. restoration scenario)
      pwater_scenario = terra::lapp(c(pwater_new, pwater),
                             function(x, y) {
                               ifelse(!is.na(x) & x > y, x, y)
                             })
    } else {
      pwater_scenario = terra::cover(pwater_new, pwater)
    }
  } else {
    # just write final baseline pwater to appropriate directory
    pwater_scenario = pwater
  }

  # write unmasked version for focal stats
  create_directory(file.path(pathout[1], scenario_name))
  terra::writeRaster(pwater_scenario,
                     file.path(pathout[1], scenario_name, 'pwater.tif'),
                     wopt = list(names = 'pwater'), overwrite = overwrite)

  # write masked or unmasked version as a direct model predictor
  if (!is.null(maskpath)) {
    # use masked version as a direct predictor
    pwater_scenario = terra::mask(pwater_scenario, rast(maskpath))
  }
  create_directory(file.path(pathout[2], scenario_name))
  terra::writeRaster(pwater_scenario,
                     file.path(pathout[2], scenario_name, 'pwater.tif'),
                     wopt = list(names = 'pwater'), overwrite = overwrite)

}
