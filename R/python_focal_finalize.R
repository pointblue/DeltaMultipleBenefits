#' Final processing of focal stats for SDMs
#'
#' Renames and rescales output from [python_focal()] as needed to match expected
#' inputs for species distribution models (SDMs). Includes options to mask by
#' another raster and fill missing values with zero.
#'
#' @details Function expects source files to be in a directory structure created
#'   by [python_focal()], which is also used to inform the final processing
#'   steps: `pathin/scenario_name/SDM/scale`. All .tif files in this source
#'   directory will be read in, and optionally masked by the raster at
#'   `maskpath`. If `cover = TRUE`, pixels in `maskpath` with a value of 1 will
#'   also be replaced with a value of 0, and passed to [terra::cover()] to fill
#'   in missing values in source data with zero.
#'
#'   If `SDM = "riparian"`, pixel counts are converted to a proportion of the
#'   total number of cells expected within the buffer distance represented by
#'   `scale`, and the `scale` is appended to the predictor name in the format
#'   "_50" or "_2000", as expected by the riparian SDMs.
#'
#'   If `SDM = "waterbird_fall"` or `SDM = "waterbird_win"`, the `scale` is
#'   appended to the predictor name in the format "_2k", "_5k", or "_10k", as
#'   expected by the waterbird SDMs.
#'
#'   The final rasters are then written to the directory `pathout/scenario`,
#'   which will be created if it doesn't yet exist.
#'
#' @param pathin,landscape_name Character strings defining the filepath
#'   (`pathin/landscape_name`) containing input rasters to be processed, such as
#'   those created from running [python_focal_run()]
#' @param SDM Character string; the name of intended species distribution model
#'   and subdirectory within `pathin/landscape_name`: `"riparian"`,
#'   `"waterbird_fall"`, or `"waterbird_win"`.
#' @param scale Character string; Spatial scale over which focal stats are
#'   summarized, and subdirectory within `pathin/landscape_name/SDM`: `50`,
#'   `2000`, `5000`, or `10000`.
#' @param pathout Character string; Filepath to directory where output rasters
#'   should be written; passed to [terra::writeRaster()]
#' @param overwrite Logical; passed to [terra::writeRaster()]
#' @param maskpath Optional filepath to a raster that should be used to mask the
#'   output, e.g. a study area boundary
#' @param cover Logical; default is `FALSE`. If `TRUE`, `maskpath` must not be
#'   NULL; See Details.
#'
#' @return Nothing returned to R environment. Writes rasters to `pathout` for
#'   each land cover class.
#' @seealso [python_focal_prep()], [python_focal_run()]
#' @export
#'
#' @examples
#' # See vignette
#'
python_focal_finalize = function(pathin, landscape_name, SDM, scale, pathout,
                                 overwrite = FALSE, maskpath = NULL,
                                 cover = FALSE) {

  # troubleshooting
  if (cover & is.null(maskpath)) {
    stop('cover=TRUE but maskpath not provided')
  }

  dat = list.files(file.path(pathin, landscape_name, SDM, scale),
                   pattern = '.tif$', full.names = TRUE) %>% terra::rast()

  if (!is.null(mask)) {
    mask = terra::rast(maskpath)
    dat = terra::mask(dat, mask)

    if (cover) { # fill NAs within mask boundary with zero (e.g. pfld)
      dat = terra::cover(dat, mask %>% terra::subst(from = 1, to = 0))
    }
  }

  if (SDM == 'riparian') {
    # fix layer names to match model predictors
    terra::names(dat) = paste0(terra::names(dat), '_', scale)

    # convert to proportion of total possible number of cells
    if (scale == '50') {
      dat = dat/13
    } else if (scale == '2000') {
      dat = dat/14073
    }

  } else if (SDM %in% c('waterbird_fall', 'waterbird_win')) {
    # fix layer names to match model predictors
    terra::names(dat) = paste0(terra::names(dat), '_', as.numeric(scale)/1000, 'k')
  }

  create_directory(file.path(pathout, landscape_name))
  terra::writeRaster(dat,
                     paste0(file.path(pathout, landscape_name), '/',
                            terra::names(dat), '.tif'),
                     overwrite = overwrite)
}
