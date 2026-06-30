#' Run focal statistics via Python
#'
#' Internal function that checks for the availability of Python, imports the
#' arcpy module and Spatial Analyst extensions, then sources the Python script
#' "focal_stats.py".
#'
#' @details This function is primarily for internal use by
#'   [python_focal_stats()] to calculate focal statistics for each land cover
#'   predictor, passing the appropriate moving window sizes and summary
#'   functions required by each species distribution model. For each unique
#'   directory of land cover predictors, this function calls the
#'   `focal_stats.py` script which reads in all rasters in the directory (unless
#'   the `regex` argument is used) and then summarizes cell values for each
#'   input raster within a buffer distance defined by `scale`. Optionally, the
#'   results can be masked after calculating focal statistics, such as to limit
#'   results to a study area.
#'
#'   Summary functions may include `'SUM'`, `'MEAN'`, or `'MAXIMUM'`. Note that
#'   the MEAN of binary land cover presence data is equivalent to the proportion
#'   cover of each land cover class within the buffer distance while the SUM
#'   represents the count of pixels within the buffer distance.
#'
#' @param pathin,SDM,landscape_name Character strings defining the filepath
#'   (`pathin/SDM/landscape_name`) containing input rasters to be processed,
#'   such as those written from running [python_focal_prep()]
#' @param regex Optional regular expression to process only a subset of the
#'   rasters in `pathin/SDM/landscape_name`
#' @param scale String representing the buffer size (in m) within which focal
#'   stats are calculated
#' @param fun Function to summarize focal statistics: `'MEAN'` or `'SUM'`
#' @param dir Filepath for the directory where output rasters should be written
#'   (as `dir/SDM/landscape_name/scale`)
#' @param python Optional filepath to the preferred version of arcpy, passed to
#'   `reticulate::use_python`. See details.
#' @param overwrite logical; allow Python to overwrite output?
#' @param mask currently experimental
#'
#' @return Nothing returned to R environment. Writes rasters to `pathout` for
#'   each land cover class.
#' @seealso [python_focal_prep()], [python_focal_stats()]
#' @keywords internal
#'
#' @examples
#' # See vignette

python_focal_run = function(pathin, landscape_name, SDM, regex = NULL,
                            scale, suffix, fun = 'MEAN', dir, python = NULL,
                            overwrite = TRUE, mask = NULL) {

  # import arcpy if not already
  if (is.null(.py_state$arcpy)) {
    if (is.null(python)) {
      # try this as the default:
      python = 'C:/Program Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3/python.exe'
    }
    .py_shared_init(python)
  }

  # load the focal_stats.py script
  env_py <- load_py_script('focal_stats')

  # create necessary directories
  fullpathin = file.path(pathin, SDM, landscape_name)
  fullpathout = file.path(dir, SDM, landscape_name)
  cat('Processing scale "', scale, '" for landscape "', landscape_name, '"\n')

  # run focal_stats.py
  # env_py$focal_stats(fullpathin = fullpathin, fullpathout = fullpathout,
  #                    buffer = scale, fun = fun, suffix = suffix, regex = regex,
  #                    overwrite = overwrite)
  # handle mask - currently experimental
  if (!is.null(mask)) {
    if (is(mask, 'SpatRaster')) {
      tmp <- tempfile(fileext = ".tif")
      print(tmp)
      terra::writeRaster(mask, tmp)
      .py_state$arcpy$MakeRasterLayer_management(tmp, "mask_layer")
      env_py$focal_stats(fullpathin = fullpathin, fullpathout = fullpathout,
                         buffer = scale, fun = fun, suffix = suffix, regex = regex,
                         overwrite = overwrite, mask_raster = "mask_layer")
    } else {
      mask_layer <- .py_state$arcpy$Raster(mask)
      env_py$focal_stats(fullpathin = fullpathin, fullpathout = fullpathout,
                         buffer = scale, fun = fun, suffix = suffix, regex = regex,
                         overwrite = overwrite, mask_raster = "mask_layer")
    }
  } else {
    env_py$focal_stats(fullpathin = fullpathin, fullpathout = fullpathout,
                       buffer = scale, fun = fun, suffix = suffix, regex = regex,
                       overwrite = overwrite)
  }

}


