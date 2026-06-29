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
#'   or a stream). Optionally, results can be scaled, masked, and written to
#'   `dir/SDM/landscape_name/filename`.
#'
#'   Currently supported scale options include: `km` to divide the results by
#'   1000 and return distances in kilometers or `sqrt` to take the square root
#'   of the results.
#'
#'   The parameters `dir`, `SDM`, `landscape_name`, and `filename` are passed to
#'   [file.path()] to contstruct filenames passed to [terra::writeRaster()]. If
#'   multiple values for one of these character strings is provided, the output
#'   raster will be written to more than one directory, e.g. if needed as a
#'   predictor for multiple SDMs or landscape names.
#'
#'   This function relies on the availability of `arcpy` and Spatial Analyst
#'   extensions. An attempt will be made to load these the first time this
#'   function (or [python_focal_stats()]) is called in each session, and by
#'   default will look here: `C:/Program
#'   Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3/python.exe`; use the
#'   `python` argument to specify a different pathway.
#'
#' @param x Filepath or SpatRaster to be processed
#' @param scale Optional character string for scaling the results; See Details
#' @param mask Optional `SpatRaster` or character string giving the filepath to
#'   a raster that should be used to mask the output, e.g. a study area boundary
#' @param python Optional filepath to the preferred version of arcpy, passed to
#'   `reticulate::use_python`; See details.
#' @param dir,SDM,landscape_name Optional; Character strings defining the
#'   filepath where output raster should be written (`dir/SDM/landscape_name`)
#' @param filename name of the output raster, including file extension; default
#'   is 'droost_km.tif', the name of the predictor required by the waterbird
#'   models
#' @param ... Additional arguments passed to [terra::writeRaster()]
#'
#' @return SpatRaster
#' @export
#'
#' @examples
#' # See vignette

python_dist = function(x, scale = NULL, mask = NULL, python = NULL,
                       dir = NULL, SDM = NULL, landscape_name = NULL,
                       filename = 'droost_km.tif', ...) {

  # import arcpy if not already
  if (is.null(.py_state$arcpy)) {
    if (is.null(python)) {
      # try this as the default:
      python = 'C:/Program Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3/python.exe'
    }
    .py_shared_init(python)
  }

  if (is(x, 'SpatRaster')) {
    filepath <- tempfile(fileext = ".tif")
    terra::writeRaster(x, filepath)
  } else if (!is(x, 'character')) {
    stop("`x` must be a character string or SpatRaster")
  }
  python_tmpfile =  tempfile(fileext = ".tif")

  # run dist_stats.py to calculate distance to roosts
  env_py <- load_py_script('dist_stats')
  env_py$dist_stats(filename = basename(filepath),
                    fullpathin = dirname(filepath) |> normalizePath(),
                    fullpathout = python_tmpfile)

  # return result for further processing:
  r = terra::rast(python_tmpfile)

  if (!is.null(scale)) {
    if (scale == 'km') {
      r = r / 1000
    } else if (scale == 'sqrt') {
      r = sqrt(r)
    }
  }

  if (!is.null(mask)) {
    if (is(mask, 'character')) {
      mask = terra::rast(mask)
    } else if (!is(mask, 'SpatRaster')) {
      stop('function expects "mask" to be either a character string or a SpatRaster')
    }
    r = terra::mask(r, mask)
  }

  names(r) = gsub('.tif', '', filename)

  if (!is.null(dir)) {

    dots <- list(...)
    keep <- intersect(names(dots), c("wopt", "gdal", "datatype", "filetype", "overwrite"))
    dots2 <- dots[keep]

    f = file.path(dir, SDM, landscape_name) # may be one or more filepaths
    create_directory(f)
    filepaths = file.path(f, filename)

    purrr::walk(filepaths, ~terra::writeRaster(r, filename = .x, !!!dots2))
  }
  return(r)
}

