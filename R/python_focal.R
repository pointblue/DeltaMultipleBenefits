#' Run focal statistics via Python
#'
#' Function to call python script for calculating focal statistics on landscape
#' rasters via arcpy.
#'
#' Calls the [focal_stats.py] function to summarize cell values for the input
#' raster within a buffer distance defined by `scale`. Summary functions may
#' include `'SUM'` or `'MEAN'`.
#'
#' Important: This function requires the availability of arcpy and Spatial
#' Analyst extensions. While these statistics can be entirely calculated in R,
#' arcpy is much faster. See vignette for more details.
#'
#' @param pathin Filepath for the directory containing input rasters to be
#'   processed, such as those created from running [python_focal_prep]
#' @param landscape_name Subdirectory containing the rasters for a target
#'   landscape scenario for generating focal_stats
#' @param SDM The name of intended species distribution model: `"riparian"`,
#'   `"waterbird_fall"`, or `"waterbird_win"`
#' @param regex Optional regular expression to process only a subset of the
#'   rasters in `pathin/landscape_name`
#' @param scale String representing the buffer size (in m) within which focal
#'   stats are calculated
#' @param fun Function to summarize focal statistics: `'SUM'` or `'MEAN'`
#' @param pathout Filepath for the directory where output rasters should be
#'   written
#'
#' @return Nothing; all files written to `pathout`
#' @seealso [python_dist]
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
