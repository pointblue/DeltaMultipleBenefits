#' Update waterbird and tidal marsh bird predictors: covertype and LANDCOVER
#'
#' Helper function for updating `covertype` and `LANDCOVER` predictors for the
#' waterbird ("waterbird_fall", "waterbird_win") and tidal marsh ("tima")
#' distribution models, respectively.
#'
#' @details Classifies land cover rasters to generate categorical predictors
#'   required by the "waterbird_fall", "waterbird_win", and "tima" models. The
#'   input raster `x` should already represent land cover predictors required by
#'   the selected `SDM`, i.e. output from [classify_landcover.SpatRaster()]. A
#'   `mask` raster can optionally be provided, either as a SpatRaster or a
#'   filepath to a raster, to first mask the input raster(s) `x`. Output can be
#'   optionally written to `dir/SDM/landscape_name` where `landscape_name` is
#'   taken from the names of the input raster(s) `x`.
#'
#' @param x SpatRaster
#' @param SDM The name of intended species distribution model:
#'   `"waterbird_fall"`, `"waterbird_win"`, or `"tima"`
#' @param mask Optional SpatRaster or string representing filepath to a raster
#'   that should be used to mask the output, e.g. a study area boundary
#' @param dir Optional string representing directory passed to
#'   [terra::writeRaster()], as (`dir/SDM/landscape_name`). See Details.
#' @param overwrite logical. If `TRUE`, output is overwritten
#' @param ... additional arguments passed to [terra::writeRaster()]
#'
#' @returns SpatRaster with the same number of layers as the input `x`, names
#'   required by the selected `SDM`.
#' @seealso [classify_landcover.SpatRaster()]; [update_pwater()];
#'   [update_roosts()]
#' @export
#'
#' @examples
#' # See vignette

update_covertype = function(x, SDM, mask = NULL, dir = NULL,
                            overwrite = FALSE, ...) {


  if (!is.null(mask)) {
    if (is(mask, 'character')) {
      mask = terra::rast(mask)
    } else if (!is(mask, 'SpatRaster')) {
      stop('function expects "mask" to be either a character string or a SpatRaster')
    }
    x = terra::mask(x, mask)
  }

  if (SDM == 'waterbird_fall') {
    covertype = terra::classify(
      x,
      rcl = data.frame(from = c(17, 4, 3, 8),
                       becomes = c(1, 2, 3, 4)) |>
        as.matrix(),
      others = NA)
    newlevels = list(
      data.frame(value = c(1:4),
                 label = c('Alfalfa', 'Irrigated pasture', 'Rice', 'Wetland')))
    levels(covertype) <- rep(newlevels, terra::nlyr(covertype))
    names(covertype) = rep('covertype', terra::nlyr(covertype))

  } else if (SDM == 'waterbird_win') {
    covertype = terra::classify(
      x,
      rcl = data.frame(from = c(17, 2, 4, 3, 8, 5),
                       becomes = c(1, 2, 3, 4, 5, 6)) |>
        as.matrix(),
      others = NA)
    newlevels = list(
      data.frame(value = c(1:6),
                 label = c('Alfalfa', 'Corn', 'Irrigated pasture', 'Rice', 'Wetland', 'Winter wheat')))
    levels(covertype) <- rep(newlevels, terra::nlyr(covertype))
    names(covertype) = rep('covertype', terra::nlyr(covertype))

  } else if (SDM == 'tima') {
    covertype = terra::classify(
      x,
      rcl = data.frame(from = c(80, 89, 190, 70, 170, 90, 20, 40),
                       to = c(83, 89, 220, 77, 187, 92, 28, 56),
                       becomes = c(1, 1, 1, 2, 2, 3, 4, 4)) |>
        as.matrix(),
      others = NA)
    newlevels = list(
      data.frame(value = c(1:4),
                 label = c('WETLAND', 'RIPARIAN', 'WATER', 'AGGRPAS')))
    levels(covertype) <- rep(newlevels, terra::nlyr(covertype))
    names(covertype) = rep('LANDCOVER', terra::nlyr(covertype))

  }

  if (!is.null(dir)) {
    purrr::map(names(x),
               function(landscape_name) {
                 create_directory(file.path(dir, SDM, landscape_name))
                 terra::writeRaster(covertype,
                                    file.path(dir, SDM, landscape_name,
                                              paste0(names(covertype),'.tif')),
                                    overwrite = overwrite, ...)
               })
  }
  return(covertype)
}
