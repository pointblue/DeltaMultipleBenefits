#' Update waterbird predictors: pwater & pfld
#'
#' Helper function for updating `pwater` and `pfld` predictors for the waterbird
#' distribution models. Generates file `pwater.tif` at locations
#' `pathout/pwater/landscape_name` and `pathout/SDM/landscape_name`.
#'
#' @details The waterbird distribution models incorporate information about
#'   surface water data in two ways: as `pwater`, the expected probability of
#'   open surface water in each cell of the landscape raster, specific to the
#'   waterbird season being modeled and perhaps averaged over multiple years,
#'   and as `pfld` focal statistics which represent the proportion of each land
#'   cover class within a given distance of each cell that is flooded (see
#'   [python_focal_prep()] and [python_focal_run()]). Therefore, `pwater` data
#'   must be available for every landscape under analysis before the `pfld`
#'   focal statistics can be generated and distribution models fit.
#'
#'   Due to the dual needs for generating `pwater` and `pfld` predictors, this
#'   function writes results in two places within `pathout`. The first will be
#'   written to `pathout/pwater/landscape_name`, intended for later use with
#'   [python_focal_prep()] and generating `pfld` predictors. The second will be
#'   written to `pathout/SDM/landscape_name`, which is expected to be a
#'   directory containing all final predictors for later use with [fit_SDM()] in
#'   fitting waterbird models.
#'
#'   In addition, this function has two modes of operation. If
#'   `scenario_landscape` is not provided, the `waterdat` is assumed to to
#'   represent `pwater` data for the `landscape_name`, and is simply renamed and
#'   copied to both `pathout` locations for use in later steps of analysis,
#'   optionally masking before `pathout/SDM/landscape_name` is written. The
#'   `mask` is never applied to the `pathout/pwater/landscape_name` output
#'   intended for later focal statistics to avoid errors in processing near the
#'   boundaries of the study area.
#'
#'   Alternatively, in the second mode, if both `baseline_landscape` and
#'   `scenario_landscape` rasters are provided, this function will estimate
#'   new `pwater` values for cells in the `scenario_landscape` that have changed
#'   cover class, based on the mean probability of open surface water for that
#'   land cover class in the `baseline_landscape`. Optionally, if `floor =
#'   TRUE`, new probabilities of open water will be assigned only if they are
#'   higher than the baseline values. In this mode, the result represents
#'   `pwater` for the `scenario_landscape`, and `landscape_name` should reflect
#'   the name of the scenario.
#'
#'   The original `pwater` baseline data used in the development of these models
#'   was derived from Point Blue's [Water
#'   Tracker](https://www.pointblue.org/autowater) and may be downloaded from
#'   [link TBD](https://doi.org).
#'
#' @param waterdat `SpatRaster` or character string giving the filepath to a
#'   raster representing the probability of open water (pwater) in each cell,
#'   specific to the time frames appropriate to each `scenario_landscape` and
#'   waterbird SDM (i.e., fall vs. winter)
#' @param mask Optional `SpatRaster` or character string giving the filepath to
#'   a raster that should be used to mask the output, e.g. a study area boundary
#' @param pathout,SDM,landscape_name Character strings defining the filepath
#'   (`pathout/SDM/landscape_name`) where output rasters should be written;
#'   landscape_name should either correspond to the landscape represented by
#'   `waterdat` or the `scenario_landscape`, if given; see Details
#' @param overwrite Logical; passed to [terra::writeRaster()]; default `FALSE`
#' @param baseline_landscape,scenario_landscape Optional SpatRasters created by
#'   [terra::rast()] to compare with each other for estimating `pwater` for the
#'   changed portions of the `scenario_landscape`; see Details
#' @param floor Logical; if `TRUE`, don't allow new values of pwater to be lower
#'   than baseline values
#'
#' @return Nothing; all files written to `pathout`
#' @seealso [update_covertype()], [update_roosts()]
#' @export
#'
#' @examples
#' # See vignette


update_pwater = function(waterdat, mask = NULL, pathout, SDM, landscape_name,
                         overwrite = FALSE, baseline_landscape= NULL,
                         scenario_landscape = NULL, floor = FALSE) {

  # troubleshooting
  if (is(waterdat, 'character')) {
    pwater = terra::rast(waterdat)
  } else if (is(waterdat, 'SpatRaster')) {
    pwater = waterdat
  } else {
    stop('function expects "waterdat" to be either a character string or a SpatRaster')
  }

  if (!is.null(mask)) {
    # mask must be a character string or a SpatRaster
    if (!is(mask, 'character') & !is(mask, 'SpatRaster')) {
      stop('function expects "mask" to be either a character string or a SpatRaster')
    }
  }

  if (is.null(scenario_landscape)) {
    #no scenario, just treat input waterdat as equivalent to desired pwater data
    #output, for optional masking below and copying to relevant pathout
    #directories
    pwater_scenario = pwater
  } else {
    # generate new pwater values for scenario_landscape
    if (is.null(baseline_landscape)) {
      stop('scenario_landscape provided, but no baseline_landscape provided for comparison')
    }

    levels(baseline_landscape) <- NULL
    levels(scenario_landscape) <- NULL

    # compare two landscapes and find the pixels that have changed land cover class
    changes = terra::diff(c(scenario_landscape, baseline_landscape)) %>%
      terra::subst(from = 0, to = NA) %>% #no change = NA
      terra::classify(rcl = matrix(c(-Inf, Inf, 1), nrow = 1)) # all others = 1

    # calculate mean pwater by land cover class (detailed) in the baseline
    # landscape - unmasked, so including any surrounding buffer area
    mwater = terra::zonal(pwater, baseline_landscape, fun = mean, na.rm = TRUE) %>%
      setNames(c('value', 'pwater')) %>%
      tidyr::drop_na() %>%
      dplyr::left_join(terra::freq(baseline_landscape), by = 'value')

    # assign mean baseline pwater values to changed pixels in each scenario
    pwater_new = scenario_landscape %>%
      terra::mask(changes) %>% #keep only changed pixels
      #reclassify by landcover class
      terra::classify(rcl = mwater %>%
                        dplyr::select(.data$value, .data$pwater) %>%
                        as.matrix(), others = NA)

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

  }

  # write unmasked version for focal stats
  create_directory(file.path(pathout, 'pwater', landscape_name))
  terra::writeRaster(pwater_scenario,
                     file.path(pathout, 'pwater', landscape_name, 'pwater.tif'),
                     wopt = list(names = 'pwater'), overwrite = overwrite)

  # write final version as a direct model predictor, optionally masking first
  if (!is.null(mask)) {
    # use masked version as a direct predictor

    if (is(mask, 'character')) {
      mask = terra::rast(mask)
    } else if (!is(mask, 'SpatRaster')) {
      stop('function expects "mask" to be either a character string or a SpatRaster')
    }
    pwater_scenario = terra::mask(pwater_scenario, mask)
  }
  create_directory(file.path(pathout, SDM, landscape_name))
  terra::writeRaster(pwater_scenario,
                     file.path(pathout, SDM, landscape_name, 'pwater.tif'),
                     wopt = list(names = 'pwater'), overwrite = overwrite)

}
