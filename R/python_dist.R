#' Calculate Euclidean distance via Python
#'
#' Wrapper function to call a python script for calculating Euclidean distances
#' on landscape rasters. On first call this function imports the arcpy module
#' and Spatial Analyst extensions then sources the Python script
#' "dist_stats.py".
#'
#' @details This function calls the `dist_stats.py` script to calculate the
#'   Euclidean distance for all cells in the input raster without a value to the
#'   nearest cell with a value (e.g., for calculating distance to a crane roost
#'   or a stream).
#'
#'   Raw python results will be written to
#'   `pathin/landscape_name/droost_raw.tif`, and then optionally scaled and/or
#'   masked, before writing the final output to `pathout/SDM/landscape_name/`.
#'   Currently supported scale options include: `km` to divide the results by
#'   1000 and return distances in kilometers or `sqrt` to take the square root
#'   of the results. Note that the initial raw output from `dist_stats.py` to
#'   `pathin/landscape_name/droost_raw.tif` will not overwrite existing rasters;
#'   old versions must be deleted before re-running.
#'
#'   *Important:* This function requires the availability of arcpy and Spatial
#'   Analyst extensions. Use the `python` argument to specify the local pathway
#'   to arcpy, particularly if other versions of Python are installed. For
#'   example: 'C:/Program
#'   Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3/python.exe'. Note that the
#'   python argument is applied only on the first use within each session, and
#'   must be repeated in each session.
#'
#' @param pathin,landscape_name Character strings defining the filepath
#'   (`pathin/landscape_name`) where input rasters are located, such as those
#'   created from running [python_focal_prep()] or [update_roosts()]
#' @param pathout,SDM Additional character strings defining the filepath
#'   (`pathout/SDM/landscape_name`) where output raster should be written
#' @param filename name of the output raster, including file extension; default
#'   is 'droost_km.tif', the name of the predictor required by the waterbird
#'   models
#' @param scale Optional character string for scaling the results; See Details
#' @param mask Optional `SpatRaster` or character string giving the filepath to
#'   a raster that should be used to mask the output, e.g. a study area boundary
#' @param overwrite Logical; passed to [terra::writeRaster()]; does not apply to
#'   the intermediate step of writing `droost_raw.tif`
#' @param python Optional filepath to the preferred version of arcpy, passed to
#'   `reticulate::use_python`. See details.
#'
#' @return Nothing; all files written to `pathout/SDM/landscape_name`
#' @export
#'
#' @examples
#' # See vignette

python_dist = function(pathin, landscape_name, pathout, SDM,
                       filename = 'droost_km.tif', scale = NULL,
                       mask = NULL, overwrite = FALSE, python = NULL) {

  # import arcpy if not already
  if (is.null(.py_state$arcpy)) .py_shared_init(python)

  # load the focal_stats.py script
  env <- load_py_script('dist_stats')

  if (!is.null(mask)) {
    if (is(mask, 'character')) {
      mask = terra::rast(mask)
    } else if (!is(mask, 'SpatRaster')) {
      stop('function expects "mask" to be either a character string or a SpatRaster')
    }
  }

  # run dist_stats.py to calculate distance to roosts and put in same pathout[1]
  fname = list.files(file.path(pathin, landscape_name), '.tif$')

  fullpathin = file.path(pathin, landscape_name) |> normalizePath()
  fullpathout =  file.path(pathin, landscape_name, filename) |>
    normalizePath()


  env$dist_stats(filename = fname, fullpathin = fullpathin, fullpathout = fullpathout)

  # further processing:
  r = file.path(pathin, landscape_name, filename) |> terra::rast()

  if (scale == 'km') {
    r = r / 1000
  } else if (scale == 'sqrt') {
    r = sqrt(r)
  }

  if (!is.null(mask)) {
    r = terra::mask(r, mask)
  }

  # write output with final scaled/masked version
  create_directory(file.path(pathout, SDM, landscape_name))
  terra::writeRaster(r, file.path(pathout, SDM, landscape_name, filename),
                     wopt = list(names = gsub('.tif', '', filename)),
                     overwrite = TRUE)
}

