#' Run focal statistics via Python
#'
#' Function to call python script for calculating focal statistics on landscape
#' rasters via arcpy.
#'
#' @details This function calls the `focal_stats.py` function to summarize cell
#'   values for the input raster within a buffer distance defined by `scale`.
#'   Summary functions may include `'SUM'` or `'MEAN'`. The default of `fun =
#'   SUM'` is intended to be called only after first running
#'   [python_focal_prep()], which prepares rasters representing the
#'   presence/absence of individual land cover classes, as defined by each set
#'   of species distribution models, and allows 'SUM' to effectively count the
#'   number of pixels of each land cover class within a given distance. This
#'   function can also be used with `fun = 'MEAN'` to estimate the mean
#'   probability of open water for a given land cover class within a given
#'   distance (i.e. _pfld predictors for waterbird models). See vignette.
#'
#'   Important: This function requires the availability of arcpy and Spatial
#'   Analyst extensions. While these statistics can be entirely calculated in R,
#'   arcpy is much faster. See vignette for more details.
#'
#' @param pathin,landscape_name Character strings defining the filepath
#'   (`pathin/landscape_name`) containing input rasters to be processed, such as
#'   those created from running [python_focal_prep()]
#' @param SDM Character string; the name of intended species distribution model
#'   and subdirectory within `pathin/landscape_name`: `"riparian"`,
#'   `"waterbird_fall"`, or `"waterbird_win"`
#' @param regex Optional regular expression to process only a subset of the
#'   rasters in `pathin/landscape_name/SDM`
#' @param scale String representing the buffer size (in m) within which focal
#'   stats are calculated
#' @param fun Function to summarize focal statistics: `'SUM'` or `'MEAN'`
#' @param pathout Filepath for the directory where output rasters should be
#'   written
#'
#' @return Nothing returned to R environment. Writes rasters to `pathout` for
#'   each land cover class.
#' @seealso [python_focal_prep()], [python_focal_finalize()]
#' @export
#'
#' @examples
#' # See vignette

python_focal_run = function(pathin, landscape_name, SDM, regex = NULL,
                        scale, fun = 'SUM', pathout) {

  arcpy <- reticulate::import('arcpy')
  arcpy$CheckOutExtension("Spatial")
  reticulate::source_python(system.file("python", "focal_stats.py",
                                        package = "DeltaMultipleBenefits"))
  # create necessary directories
  create_directory(file.path(pathout, landscape_name, SDM, scale))

  # run focal_stats.py
  focal_stats(
    pathin = file.path(pathin, landscape_name, SDM),
    pathout = file.path(pathout, landscape_name, SDM, scale),
    buffer = scale, fun = fun, regex = regex)

}
