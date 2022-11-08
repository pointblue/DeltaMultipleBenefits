#' Update waterbird predictors: pwater
#'
#' Helper function for updating pwater predictors for the waterbird distribution
#' models.
#'
#' @details Uses surface water data in `waterdatpath` to extract cell-specific
#'   probabilities of open water for the `baseline_landscape`; if
#'   `scenario_landscape` is also provided, representing a projected change from
#'   the baseline, assign new cell-specific probabilities of surface water for
#'   the cells that have changed class, based on the mean probability of open
#'   water for each land cover class in the baseline landscape. If `floor =
#'   TRUE`, new probabilities of open water will be assigned only if they are
#'   highthan the baseline values. Generates file: `pwater.tif'`. Use this
#'   function prior to using [python_focal_prep()] with `mask` = 'pwater.tif' to
#'   generate probability of flooding data by land cover class for use with the
#'   waterbird SDMs.
#'
#' @param landscape SpatRaster created by [terra::rast()]
#' @param scenario_landscape Optional secondary SpatRaster created by
#'   [terra::rast()] to compare against landscape in [generate_pwater()]; see
#'   Details.
#' @param pathout Character string; Filepath to directory where output rasters
#'   should be written; passed to [terra::writeRaster()]
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
                           waterdatpath, maskpath, floor = TRUE) {

  pwater = rast(waterdatpath)
  if (!is.null(maskpath)) {
    pwater = mask(pwater, rast(maskpath))
  }

  if (!is.null(scenario_landscape)) {
    #generate new pwater values for scenario
    # calculate mean baseline pwater by land cover class (detailed)
    mwater = zonal(pwater, landscape, fun = mean, na.rm = TRUE) %>%
      setNames(c('label', 'pwater')) %>% drop_na() %>%
      left_join(freq(landscape), by = 'label')

    # assign mean baseline pwater values to changed pixels in each scenario
    changes = diff(c(scenario_landscape, landscape)) %>%
      subst(from = 0, to = NA) %>% #no change = NA
      classify(rcl = matrix(c(-Inf, Inf, 1), nrow = 1)) # all others = 1

    pwater_new = scenario_landscape %>% mask(changes) %>%
      classify(rcl = mwater %>% select(value, pwater) %>% as.matrix(),
               othersNA = TRUE)

    if (floor) {
      # current mean pwater is the "floor"; only allow pwater to increase for
      # changed pixels (e.g. restoration scenario)
      pwater_scenario = lapp(c(pwater_new, pwater),
                             function(x, y) {
                               ifelse(!is.na(x) & x > y, x, y)
                             })
    } else {
      pwater_scenario = cover(pwater_new, pwater)
    }
  } else {
    # just write final baseline pwater to appropriate directory
    pwater_scenario = pwater
  }

  create_directory(file.path(pathout, scenario_name))
  writeRaster(pwater_scenario,
              file.path(pathout, scenario_name, 'pwater.tif'),
              wopt = list(names = 'pwater'), overwrite = overwrite)

}
