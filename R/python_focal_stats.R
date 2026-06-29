#' Run focal statistics via Python
#'
#' Wrapper function to call a python script for calculating focal statistics on
#' land cover predictors for species distribution models. On first call this
#' function imports the arcpy module and Spatial Analyst extensions then sources
#' the Python script "focal_stats.py".
#'
#' @details This function is designed to be called after running
#'   [python_focal_prep()], and writing the resulting rasters representing land
#'   cover predictors to `pathin/SDM/landscape_names`. For each raster in these
#'   directories, this function calculates focal statistics required for each
#'   species distribution model, on the appropriate spatial scales and with the
#'   required summary statistics. The `regex` argument provides options for
#'   processing only a subset of the rasters in the directory. If
#'   `overwrite=TRUE` (the default), previously-created focal statistics will be
#'   overwritten. If `mask` provides a filepath to a raster, the results will be
#'   masked by this file before writing to file.
#'
#'   These calculations can be very slow, depending on the size and resolution
#'   of the rasters, and relies on the availability of `arcpy` and Spatial
#'   Analyst extensions for faster processing. An attempt will be made to load
#'   these the first time this function (or [python_dist()]) is called in each
#'   session, and by default will look here: `C:/Program
#'   Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3/python.exe`; use the
#'   `python` argument to specify a different pathway.
#'
#' @param pathin,SDM,landscape_names Character strings defining the filepath
#'   (`pathin/SDM/landscape_names`) containing input rasters to be processed,
#'   such as those written from running [python_focal_prep()]
#' @param dir Filepath for the directory where output rasters should be written
#'   (as `dir/SDM/landscape_name/scale`)
#' @param regex Optional regular expression to process only a subset of the
#'   rasters in `pathin/SDM/landscape_name`
#' @param python Optional filepath to the preferred version of arcpy, passed to
#'   `reticulate::use_python`. See details.
#' @param overwrite logical; allow Python to overwrite existing output?
#' @param mask currently experimental
#'
#' @return None
#' @seealso [python_focal_prep()], [fit_SDM()]
#' @export
#'
#' @examples
#' # See vignette

python_focal_stats = function(SDM, pathin, dir, landscape_names,
                              regex = NULL, python = NULL, overwrite = TRUE,
                              mask = NULL) {

  fullpathout = file.path(dir, SDM, landscape_names)
  create_directory(fullpathout)

  if (SDM == 'tima') {
    # automatically choose the correct scales and function and put the purrr in here
    df = tidyr::expand_grid(landscape_name = landscape_names,
                            scale = c('100', '2000')) |>
      dplyr::mutate(suffix = scale)

    if (!is.null(regex)) {
      if (regex == 'PSIZE.tif') {
        res = purrr::pmap(df,
                    function(landscape_name, scale, suffix, ...) {
                      python_focal_run(
                        pathin = pathin,
                        landscape_name = landscape_name,
                        SDM = SDM,
                        regex = regex,
                        scale = scale,
                        suffix = suffix,
                        fun = 'MAXIMUM',
                        dir = dir,
                        python = python,
                        overwrite = overwrite,
                        mask = mask)})
      }
    } else {
      res = purrr::pmap(df,
                  function(landscape_name, scale, suffix, ...) {
                    python_focal_run(
                      pathin = pathin,
                      landscape_name = landscape_name,
                      SDM = SDM,
                      regex = regex,
                      scale = scale,
                      suffix = suffix,
                      fun = 'MEAN',
                      dir = dir,
                      python = python,
                      overwrite = overwrite,
                      mask = mask)})
    }


  } else if (SDM == 'riparian') {
    # automatically choose the correct scales and put the purrr in here
    df = tidyr::expand_grid(landscape_name = landscape_names,
                            scale = c('50', '2000')) |>
      dplyr::mutate(suffix = paste0('_', scale))

    res = purrr::pmap(df,
                function(landscape_name, scale, suffix, ...) {
                  python_focal_run(
                    pathin = pathin,
                    landscape_name = landscape_name,
                    SDM = SDM,
                    regex = regex,
                    scale = scale,
                    suffix = suffix,
                    fun = 'MEAN',
                    dir = dir,
                    python = python,
                    overwrite = overwrite,
                    mask = mask)})

  } else if (SDM %in% c('waterbird_fall', 'waterbird_win')) {

    if (SDM == 'waterbird_fall') {
      df = tidyr::expand_grid(landscape_name = landscape_names,
                              scale = c('2000', '5000', '10000')) |>
        dplyr::mutate(suffix = paste0('_', as.numeric(scale)/1000, 'k'))
    } else if (SDM == 'waterbird_win') {
      df = tidyr::expand_grid(landscape_name = landscape_names,
                              scale = c('5000', '10000')) |>
        dplyr::mutate(suffix = paste0('_', as.numeric(scale)/1000, 'k'))
    }

    if (!is.null(regex)) {
      regex1 = paste0(regex, '_area.tif')
      regex2 = paste0(regex, '_pfld.tif')
    } else {
      regex1 = '*_area.tif'
      regex2 = '*_pfld.tif'
    }

    cat('Calculating focal statistics for the area of each land cover class\n')
    res = purrr::pmap(df,
                function(landscape_name, scale, suffix, ...) {
                  python_focal_run(
                    pathin = pathin,
                    landscape_name = landscape_name,
                    SDM = SDM,
                    regex = regex1,
                    scale = scale,
                    suffix = suffix,
                    fun = 'SUM',
                    dir = dir,
                    python = python,
                    overwrite = overwrite,
                    mask = mask)})

    cat('Calculating focal statistics for the proportion of each land cover class with surface water\n')
    res2 = purrr::pmap(df,
                function(landscape_name, scale, suffix, ...) {
                  python_focal_run(
                    pathin = pathin,
                    landscape_name = landscape_name,
                    SDM = SDM,
                    regex = regex2,
                    scale = scale,
                    suffix = suffix,
                    fun = 'MEAN',
                    dir = dir,
                    python = python,
                    overwrite = overwrite,
                    mask = mask)})

  }
  return(invisible(NULL))
}
