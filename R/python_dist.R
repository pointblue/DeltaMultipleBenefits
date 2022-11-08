#' Calculate Euclidean distance via Python
#'
#' Function to call python script for calculating Euclidean distances on
#' landscape rasters via arcpy.
#'
#' Calls the [dist_stats.py] function to calculate the Euclidean distance (in
#' km) for all cells in the input raster without a value to the nearest cell
#' with a value (e.g., for calculating distance to a crane roost or a stream).
#'
#' Important: This function requires the availability of arcpy and Spatial
#' Analyst extensions. While these statistics can be entirely calculated in R,
#' arcpy is much faster. See vignette for more details.
#'
#' @param pathin Filepath for the directory containing input rasters to be
#'   processed, such as those created from running [python_focal_prep]
#' @param landscape_name Subdirectory containing the rasters for a target
#'   landscape scenario for generating focal_stats
#' @param copyto Optional character string containing alternate `landscape_name`
#'   to which the results of [python_dist()] should be copied.
#' @param pathout filepath for the directory where output rasters should be
#'   written
#' @param filename name of resulting layer, including file extension; default is
#'   'dist.tif'
#' @param maskpath Optional filepath to a raster that should be used to mask the
#'   output, e.g. a study area boundary
#' @param overwrite Logical; passed to [terra::writeRaster()]. Applies to
#'   `copyto` only.
#'
#' @return Nothing; all files written to `pathout`
#' @seealso [python_focal()]
#' @export
#'
#' @examples
#' # See vignette

python_dist = function(pathin, landscape_name, copyto = NULL, pathout,
                       filename = 'dist.tif', maskpath = NULL,
                       overwrite = FALSE) {

  # calculate distance to roosts and put in same pathin directory
  dist_stats(filename = 'roosts.tif',
             fullpathin = file.path(pathin, landscape_name) %>% normalizePath(),
             fullpathout =  file.path(pathin, landscape_name) %>%
               normalizePath() %>% paste0('\\', filename))

  r = file.path(pathin, landscape_name, filename) %>% rast()

  if (!is.null(maskpath)) {
    r = mask(r, rast(maskpath))
  }
  create_directory(file.path(pathout[1], landscape_name))
  writeRaster(r, file.path(pathout[1], landscape_name, filename),
              wopt = list(names = gsub('.tif', '', filename)),
              overwrite = overwrite)

  if (!is.null(copyto)) {
    # copy from scenario_name/pathout[1] to copyto/pathout[2]
    create_directory(file.path(pathout[2], copyto))
    writeRaster(r, file.path(pathout[2], copyto, filename),
                wopt = list(names = gsub('.tif', '', filename)),
                overwrite = overwrite)
  }
}
