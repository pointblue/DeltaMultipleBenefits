#' Calculate the difference between predictions of species presence for a
#' baseline and scenario landscape
#'
#' @details This function is designed to handle predictions for multiple species
#'   provided as rasters in the `baseline` and `scenario` directories, matched
#'   by name. The difference is calculated as the predicted value for the
#'   scenario minus the predicted value for the baseline, such that positive
#'   values in the result represent an increased probability of presence, and
#'   negative values represent a reduced probability of presence.
#'
#'   If `differentiate = TRUE`, the function is also designed to distinguish
#'   between two types of zero values in the result: locations where the
#'   predicted value is the same in both landscapes (which will retain a value
#'   of zero difference), and locations where the predicted value is zero in
#'   both landscapes (which will be converted to NA).
#'
#'
#' @param pathin,SDM Character strings defining the filepath (`pathin/SDM`)
#'   containing the predicted probability of presence resulting from each
#'   distribution model, such as those created from running [fit_SDM()] and
#'   [transform_SDM()]; SDM must be one of `"riparian"`, `"waterbird_fall"`, or
#'   `"waterbird_win"`.
#' @param baseline_name Character string defining the filepath
#'   (`pathin/SDM/baseline`) containing the predictions for a baseline landscape
#' @param scenario_name Character string defining the filepath
#'   (`pathin/SDM/scenario`) containing the predictions for a scenario landscape
#' @param pathout Character string defining the filepath
#'   (`pathout/SDM/scenario`) where the results will be written
#' @param overwrite Logical; passed to [terra::writeRaster()]; default `FALSE`
#' @param differentiate Logical; if `TRUE`, locations where the predicted value
#'   is zero in both landscapes will be converted to NA
#' @return
#' @export
#'
#' @examples
#' # See vignette
#'

calc_change_SDM = function(pathin, SDM, baseline_name, scenario_name, pathout,
                           overwrite = FALSE, differentiate = TRUE) {

  baseline = list.files(file.path(pathin, SDM, baseline_name), '.tif',
                        full.names = TRUE) %>% rast()
  scenario = list.files(file.path(pathin, SDM, scenario_name), '.tif',
                        full.names = TRUE) %>% rast()

  delta = purrr::map(
    names(baseline),
    ~terra::diff(c(baseline[[.x]],
            scenario[[.x]]))) %>%
    stats::setNames(names(baseline)) %>%
    terra::rast()

  # distinguish between two types of zeroes in the resulting difference layer:
  # -- never was habitat and still isn't vs. already was habitat and still is
  if (differentiate) {
    res = terra::cover(
      delta %>% terra::subst(0, NA),
      baseline %>% terra::subst(from = c(0, 1), to = c(NA, 0)))
    } else {
    res = delta
    }

  create_directory(file.path(pathout, SDM, scenario_name))
  writeRaster(res,
              file.path(pathout, SDM, scenario_name,
                        paste0(names(baseline), '.tif')),
              overwrite = overwrite)
}
