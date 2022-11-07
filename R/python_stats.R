#' Run focal statistics and calculate distance via Python
#'
#' Functions to call python scripts for calculating focal statistics and
#' Euclidean distances on landscape rasters via arcpy.
#'
#' `python_focal` calls the `focal_stats.py` function to summarize cell values
#' for the input raster within a buffer distance defined by `scale`. Summary
#' functions may include `'SUM'` or `'MEAN'`.
#'
#' `python_dist` calls the `dist_stats.py` function to calculate the Euclidean
#' distance (in m) for all cells in the input raster without a value to the
#' nearest cell with a value (e.g., for calculating distance to a crane roost or
#' a stream).
#'
#' Important: These functions require the availability of arcpy and Spatial
#' Analyst extensions. While these statistics can be entirely calculated in R,
#' arcpy is much faster. See vignette for more details.
#'
#' @param pathin Filepath for the directory containing input rasters to be
#'   processed, such as those created from running [python_focal_prep]
#' @param landscape_name Subdirectory containing the rasters for a target
#'   landscape scenario for generating focal_stats
#' @param regex Optional regular expression to process only a subset of the
#'   rasters in `pathin`/`landscape_name`
#' @param SDM The name of intended species distribution model: `"riparian"`,
#'   `"waterbird_fall"`, or `"waterbird_win"`
#' @param scale String representing the buffer size (in m) within which focal
#'   stats are calculated
#' @param pathout For `python_focal`, filepath for the directory where output
#'   rasters should be written; for `python_dist` full filepath including
#'   specific filename ending in '.tif'
#' @param fun Function to summarize focal statistics: `'SUM'` or `'MEAN'`
#' @param copyto Optional character string containing alternate `landscape_name`
#'   to which the results of [python_dist()] should be copied.
#' @param maskpath Optional filepath to a raster that should be used to mask the
#'   output, e.g. a study area boundary
#' @param overwrite Logical; passed to [terra::writeRaster()]
#'
#' @return Nothing; all files written to `pathout`
#' @export
#'
#' @examples
#' # See vignette

python_focal = function(pathin, landscape_name, SDM, regex = NULL,
                        scale, fun = 'SUM', pathout) {

  # create necessary directories
  create_directory(file.path(pathout, landscape_name, SDM, scale))

  # run focal stats
  focal_stats(
    pathin = file.path(pathin, landscape_name, SDM),
    pathout = file.path(pathout, landscape_name, SDM, scale),
    buffer = scale, fun = fun, regex = regex)

}

python_dist = function(pathin, landscape_name, copyto = NULL, pathout,
                       maskpath = NULL, overwrite = FALSE) {

  # calculate distance to roosts and put in same pathin directory
  dist_stats(filename = 'roosts.tif',
             fullpathin = file.path(pathin, landscape_name) %>% normalizePath(),
             fullpathout =  file.path(pathin, landscape_name) %>%
               normalizePath() %>% paste0('\\droost_km.tif'))

  r = file.path(pathin, landscape_name, 'droost_km.tif') %>% rast()

  if (!is.null(maskpath)) {
    r = mask(r, rast(maskpath))
  }
  create_directory(file.path(pathout[1], landscape_name))
  writeRaster(r, file.path(pathout[1], landscape_name, 'droost_km.tif'),
              wopt = list(names = 'droost_km'), overwrite = overwrite)

  if (!is.null(copyto)) {
    # copy from scenario_name/pathout[1] to copyto/pathout[2]
    create_directory(file.path(pathout[2], copyto))
    writeRaster(r, file.path(pathout[2], copyto, 'droost_km.tif'),
                wopt = list(names = 'droost_km'), overwrite = overwrite)
  }
}
