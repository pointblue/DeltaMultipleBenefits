#' Apply species distribution models to new landscapes.
#'
#' Fit previously-developed species distribution models for riparian landbird
#' species and waterbird groups during the fall or winter to a new set of
#' predictors, such as those derived from a new scenario of landscape change.
#'
#' @details This function is designed to fit multiple distribution models to the
#'   same set of predictors describing a given landscape. New predictors must
#'   first be created and named to match the predictors included in the original
#'   models, e.g. using [python_focal_prep()], [python_focal_run()], and
#'   [python_focal_finalize()].
#'
#'   `constants` are passed to [terra::predict()] and provide a way to include
#'   constant values for one or more predictors that should be applied to all
#'   pixels. For both riparian and waterbird models, this will include a
#'   predictor representing effort ('area.ha' for riparian landbirds and
#'   'offset' for waterbirds). For riparian landbird models applied only to the
#'   Delta, constants should also include a region predictor used as a
#'   categorical predictor representing the Sacramento Valley (0) or the Delta
#'   or San Joaquin Valley (1). (See vignette)
#'
#'   `factors` are also passed to [terra::predict()] and provide a way to define
#'   categorical predictors. For waterbird models, this is necessary to define
#'   the 'covertype' predictor. (See vignette)
#'
#'   `unsuitable` land covers will be presumed to have a predicted value of
#'   zero. The locations of `unsuitable` landcovers will be extracted from
#'   `landscape`, assigned a value of zero, and overlaid on the model
#'   predictions.
#'
#' @param pathin,SDM,landscape_name Character strings defining the filepath
#'   (`pathin/SDM/landscape_name`) containing new predictor rasters to include
#'   in the model, such as those created from running [python_focal_finalize()]
#' @param modlist List of model objects of class 'gbm' representing the
#'   distribution models to which new predictors should be fit.
#' @param constants optional dataframe containing predictors with a constant
#'   value that should be applied to all pixels. See Details.
#' @param factors optinal list of named lists defining categorical predictors
#'   included in distribution models. See Details.
#' @param unsuitable optional vector of numerical values representing the land
#'   cover classifications that should be considered unsuitable *a priori*. If
#'   not `NULL`, `landcape` must also be provided.
#' @param landscape optional SpatRaster corresponding to the landscape
#'   represented by the predictors contained in `pathin/landscape_name`, used to
#'   identify the locations of `unsuitable` land covers. Must be provided if
#'   `unsuitable` is not `NULL`.
#' @param pathout Character string defining the filepath
#'   (`pathout/SDM/landscape_name`) where output rasters should be written
#' @param overwrite Logical; passed to [terra::writeRaster()]
#'
#' @return Nothing returned to R environment. Writes rasters to `pathout` for
#'   each model in `modlist`
#' @seealso [python_focal_prep()], [python_focal_run()],
#'   [python_focal_finalize()], [update_covertype()], [update_pwater()],
#'   [update_roosts()], [python_dist()]
#' @importFrom gbm predict.gbm
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @export
#'
#' @examples
#' # See vignette
#'

fit_SDM = function(pathin, SDM, landscape_name, modlist, constants = NULL,
                   factors = NULL, landscape = NULL, unsuitable = NULL,
                   pathout, overwrite = FALSE) {

  if (is.null(landscape) & !is.null(unsuitable)) {
    stop('Unsuitable cover types specified but landscape not provided')
  }
  if (!is.null(landscape) & is.null(unsuitable)) {
    warning('Landscape provided but unsuitable cover types not specified')
  }

  create_directory(file.path(pathout, SDM, landscape_name))

  # scenario-independent predictors (in pathin) and scenario-specific predictors
  predictors = c(list.files(file.path(pathin, SDM), pattern = '.tif$', full.names = TRUE),
                 list.files(file.path(pathin, SDM, landscape_name), pattern = '.tif$',
                            full.names = TRUE)) %>%
    terra::rast()

  # NOTE: This step is slow:
  if (is.null(unsuitable)) {
    #predict and write results raster directly
    purrr::map(names(modlist),
               ~terra::predict(
                 model = modlist[[.x]],
                 object = terra::subset(
                   x = predictors,
                   subset = modlist[[.x]]$contributions %>%
                     dplyr::filter(!var %in% names(constants)) %>%
                     dplyr::pull(var)),
                 n.trees = modlist[[.x]]$n.trees,
                 na.rm = TRUE,
                 type = 'response',
                 const = constants,
                 factors = factors,
                 filename = paste0(file.path(pathout, SDM, landscape_name), '/',
                                   .x, '.tif'),
                 overwrite = overwrite,
                 wopt = list(names = .x)
               ))
  } else {
    # predict results, but fill in unsuitable land covers with zero before
    # writing raster
    mask = terra::classify(landscape,
                           rcl = data.frame(from = unsuitable,
                                            to = 0) %>% as.matrix(),
                           others = NA)

    purrr::map(names(modlist),
               ~terra::predict(
                 model = modlist[[.x]],
                 object = terra::subset(
                   x = predictors,
                   subset = modlist[[.x]]$contributions %>%
                     dplyr::filter(!var %in% names(constants)) %>%
                     dplyr::pull(var)),
                 n.trees = modlist[[.x]]$n.trees,
                 na.rm = TRUE,
                 type = 'response',
                 const = constants,
                 factors = factors) %>%
                 terra::cover(
                   x = mask,
                   y = .,
                   filename = paste0(file.path(pathout, SDM, landscape_name), '/',
                                     .x, '.tif'),
                   overwrite = overwrite,
                   wopt = list(names = .x)))
  }
}
