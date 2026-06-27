
#' Estimate tidal marsh patch size
#'
#' Additional preparation required prior to running focal statistics on a
#' landscape raster via Python, to generate tidal wetland patch size estimates
#' for use with tidal marsh bird ('tima') models.
#'
#' The input should be a SpatRaster or list of SpatRasters resulting from
#' running [python_focal_prep()] with `SDM = "tima"`. This function extracts the
#' `TWET` predictor layer, representing all tidal wetland vegetation, identifies
#' distinct contiguous patches, and assigns each pixel within each patch a value
#' corresponding to the count of pixels within the patch.
#'
#' @param x SpatRaster or list of SpatRasters; see Details
#' @param directions integer passed to [terra::patches()] indicating which cells
#'   are considered adjacent. Should be 8 (Queen's case) or 4 (Rook's case)
#' @param zeroAsNA logical passed to [terra::patches()]. If TRUE treat cells
#'   that are zero as if they were NA
#' @param fill logical. If TRUE replaces all non-tidal wetland vegetation with
#'   0.
#' @param dir Optional string representing directory passed to
#'   [terra::writeRaster()], as (`dir/SDM/landscape_name`). See Details.
#' @param overwrite logical. If `TRUE`, output is overwritten
#' @param ... additional arguments passed to [terra::writeRaster()]
#'
#' @returns SpatRaster
#' @export
#'
#' @examples
#' codenums = DeltaMultipleBenefits::key$CODE_NUM
#' r <- terra::rast(matrix(sample(codenums, size = 1000, replace = TRUE), ncol = 100, nrow = 100))
#' tima_pred = python_focal_prep(r, SDM = 'tima')
#' tima_psize = estimate_tima_patchsize(tima_pred)
#'
estimate_tima_patchsize = function(x, directions = 8, zeroAsNA = TRUE,
                                   fill = FALSE,
                                   dir = NULL,
                                   overwrite = FALSE, ...) {
  if (is(x, 'list')) {
    if (!'TWET' %in% names(x[[1]])) {
      stop('Expect a layer named "TWET" or a SpatRaster with a single layer')
    } else { # extract just the TWET layers
      twet = purrr::map(x,
                     ~.x[['TWET']]) |> terra::rast()
    }
  } else if (is(x, 'SpatRaster')) {
    if (terra::nlyr(x) > 1 & !'TWET' %in% names(x)) {
      stop('Expect a layer named "TWET" or a SpatRaster with a single layer')
    } else if (terra::nlyr(x) > 1) {
      twet = x[['TWET']]
    } else {
      twet = x
    }
  }

  # ID patches, estimate size, then replace patch ID with patch size values
  p = terra::patches(twet, directions = directions, zeroAsNA = zeroAsNA)
  psize = terra::freq(p)
  p = terra::classify(p, rcl = psize |> dplyr::select(from = 'value', to = 'count'))

  if (fill) {
    p = cover(p, subst(twet, from = 1, to = 0))
  }

  if (!is.null(dir)) {
    names(p) = rep('PSIZE', terra::nlyr(p))
    if (is(x, 'list')) { # element names = landscape scenarios
      for (i in c(1:length(names(x)))) {
        create_directory(file.path(dir, 'tima', names(x)[i]))
        terra::writeRaster(p[[i]],
                           filename = file.path(dir, 'tima', names(x)[i],
                                                'PSIZE.tif'),
                           overwrite = overwrite, ...)
        }
    } else if (is(x, 'SpatRaster')) { # layer names = landscape scenarios
      create_directory(file.path(dir, 'tima', names(x)[i]))
      terra::writeRaster(p,
                         filename = file.path(dir, 'tima', names(x)[i],
                                              'PSIZE.tif'),
                         overwrite = overwrite, ...)
    }
  }
  return(p)
}
