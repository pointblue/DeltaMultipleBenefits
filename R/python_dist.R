#' Calculate Euclidean distance via Python
#'
#' Function to call python script for calculating Euclidean distances on
#' landscape rasters via arcpy.
#'
#' @details Calls the `dist_stats.py` function to calculate the Euclidean
#'   distance for all cells in the input raster without a value to the nearest
#'   cell with a value (e.g., for calculating distance to a crane roost or a
#'   stream).
#'
#'   Resulting distances may be scaled using the `scale` argument. Currently
#'   supported options include: `km` to divide the results by 1000 and return
#'   distances in kilometers or `sqrt` to take the square root of the results.
#'
#'   Important: This function requires the availability of arcpy and Spatial
#'   Analyst extensions. While these statistics can be entirely calculated in R,
#'   arcpy is much faster. Note: the initial output of `dist_stats.py` will not
#'   overwrite existing rasters; old versions must be deleted before re-running.
#'
#' @param pathin Filepath for the directory containing input rasters to be
#'   processed, such as those created from running [python_focal_prep()]
#' @param landscape_name Subdirectory containing the rasters for a target
#'   landscape scenario for generating focal_stats
#' @param copyto Optional character string containing alternate `landscape_name`
#'   to which the results of [python_dist()] should be copied.
#' @param pathout filepath for the directory where output rasters should be
#'   written
#' @param filename name of resulting layer, including file extension; default is
#'   'droost_km.tif', the name of the predictor required by the waterbird models
#' @param scale Optional character string for scaling the results; See Details
#' @param maskpath Optional filepath to a raster that should be used to mask the
#'   output, e.g. a study area boundary
#' @param overwrite Logical; passed to [terra::writeRaster()]. Applies to
#'   `copyto` only.
#'
#' @return Nothing; all files written to `pathout`
#' @importFrom reticulate import
#' @importFrom reticulate source_python
#' @export
#'
#' @examples
#' # See vignette

python_dist = function(pathin, landscape_name, copyto = NULL, pathout,
                       filename = 'droost_km.tif', scale = NULL,
                       maskpath = NULL, overwrite = FALSE) {

  arcpy <- reticulate::import('arcpy')
  arcpy$CheckOutExtension("Spatial")
  reticulate::source_python(system.file("python", "dist_stats.py",
                                        package = "DeltaMultipleBenefits"))

  create_directory(file.path(pathout[1], landscape_name))
  # run dist_stats.py to calculate distance to roosts and put in same pathout[1]
  fname = list.files(file.path(pathin, landscape_name), '.tif$')
  dist_stats(filename = fname,
             fullpathin = file.path(pathin, landscape_name) %>% normalizePath(),
             fullpathout =  file.path(pathout[1], landscape_name, filename) %>%
               normalizePath())

  r = file.path(pathout[1], landscape_name, filename) %>% terra::rast()

  if (scale == 'km') {
    r = r / 1000
  } else if (scale == 'sqrt') {
    r = sqrt(r)
  }

  if (!is.null(maskpath)) {
    # overwrite dist.py output in pathout[1] with masked version
    r = terra::mask(r, terra::rast(maskpath))
    terra::writeRaster(r, file.path(pathout[1], landscape_name, filename),
                       wopt = list(names = gsub('.tif', '', filename)),
                       overwrite = TRUE)
  }

  if (!is.null(copyto)) {
    # copy from scenario_name/pathout[1] to copyto/pathout[2]
    create_directory(file.path(pathout[2], copyto))
    terra::writeRaster(r, file.path(pathout[2], copyto, filename),
                       wopt = list(names = gsub('.tif', '', filename)),
                       overwrite = overwrite)
  }
}
