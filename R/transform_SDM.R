#' Transform predictions from species distribution models to binary
#'
#' Use model-specific threshold values to transform predicted probabilities of
#' species presence, such as resulting from [fit_SDM()], to binary predictions
#' of presence or absence.
#'
#' @param pathin,SDM,landscape_name Character strings defining the filepath
#'   (`pathin/SDM/landscape_name`) containing the predicted probability of
#'   presence resulting from each distribution model, such as those created from
#'   running [fit_SDM()]; SDM must be one of `"riparian"`, `"waterbird_fall"`,
#'   or `"waterbird_win"`.
#' @param modlist List of model objects of class 'gbm' representing the
#'   distribution models to which new predictors should be fit.
#' @param stat Character string defining the threshold statistic to be used; see
#'   [dismo::threshold()] for options
#' @param pathout Filepath for the directory where results rasters should be
#'   written (`pathout/SDM/landscape_name`)
#' @param overwrite Logical; passed to [terra::writeRaster()]
#'
#' @return Nothing returned to R environment. Writes rasters to
#'   `pathout/SDM/landscape_name` for each model in `modlist`
#' @seealso [fit_SDM()]
#' @importFrom dismo evaluate
#' @importFrom dismo threshold
#' @export
#'
#' @examples
#' # See vignette

transform_SDM = function(pathin, SDM, landscape_name, modlist, stat, pathout,
                         overwrite = FALSE) {

  predictions = list.files(file.path(pathin, SDM, landscape_name),
                           pattern = '.tif$', full.names = TRUE) %>%
    terra::rast()

  threshold_list = purrr::map(names(modlist) %>% setNames(names(modlist)),
                              function(x) {
                                obs = modlist[[x]]$gbm.call$dataframe[modlist[[x]]$gbm.call$gbm.y]
                                presence = modlist[[x]]$fitted[obs == 1]
                                absence = modlist[[x]]$fitted[obs == 0]
                                e = dismo::evaluate(presence, absence)
                                dismo::threshold(e, stat)
                              })

  create_directory(file.path(pathout, SDM, landscape_name))

  # reclassify using model-specific thresholds and write to file
  purrr::map(names(modlist),
             ~terra::classify(
                predictions[[.x]],
                rcl = matrix(c(-Inf, threshold_list[[.x]], 0,
                               threshold_list[[.x]], Inf, 1),
                             nrow = 2, byrow = TRUE),
                filename = paste0(file.path(pathout, SDM, landscape_name, .x),
                                  '.tif'),
                overwrite = overwrite,
                wopt = list(names = .x))
              )
}
