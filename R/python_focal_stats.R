#' Run focal statistics via Python
#'
#' Wrapper function to call a python script for calculating focal statistics on
#' land cover predictors for species distribution models. On first call this
#' function imports the arcpy module and Spatial Analyst extensions then sources
#' the Python script "focal_stats.py".
#'
#' @details This function is designed to be called after running
#'   [classify_landcover.SpatRaster()] and [python_focal_prep()], and writing
#'   the resulting rasters representing land cover predictors to
#'   `pathin/SDM/landscape_names`. For each raster in these directories, this
#'   function calls inernal functions to calculate focal statistics required
#'   for each species distribution model, on the appropriate spatial scales and
#'   with the required summary statistics. The `regex` argument provides options
#'   for processing only a subset of the rasters in the directory. If
#'   `overwrite=TRUE` (the default), previously-created focal statistics will be
#'   overwritten. If `mask` provides a filepath to a raster, the results will be
#'   masked by this file before writing to file.
#'
#'   These calculations can be very slow, depending on the size and resolution
#'   of the rasters, and relies on the availability of `arcpy` and Spatial
#'   Analyst extensions for faster processing. An attempt will be made to load
#'   these the first time this function is called in each session, and by
#'   default will look here: `C:/Program
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
#' @return Nothing returned to R environment. Writes rasters to `pathout` for
#'   each land cover class.
#' @seealso [python_focal_prep()], [fit_SDM()]
#' @export
#'
#' @examples
#' # See vignette

python_focal_stats = function(SDM, pathin, dir, landscape_names,
                              regex = NULL, python = NULL, overwrite = TRUE,
                              mask = NULL) {
  if (SDM == 'tima') {
    # automatically choose the correct scales and function and put the purrr in here
    df = tidyr::expand_grid(landscape_name = landscape_names,
                            scale = c('100', '2000')) |>
      dplyr::mutate(suffix = scale)

    purrr::pmap(df,
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

  } else if (SDM == 'riparian') {
    # automatically choose the correct scales and function and put the purrr in here
    df = tidyr::expand_grid(landscape_name = landscape_names,
                            scale = c('50', '2000')) |>
      dplyr::mutate(suffix = paste0('_', scale))

    purrr::pmap(df,
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

  } else if (SDM == 'waterbird_fall') {

    df = tidyr::expand_grid(landscape_name = landscape_names,
                     scale = c('2000', '5000', '10000')) |>
      dplyr::mutate(suffix = paste0('_', as.numeric(scale)/1000, 'k'))

    regex1 = ifelse(!is.null(regex), paste0(regex, '_area.tif'), '*_area.tif')
    purrr::pmap(df,
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

    df2 = tidyr::expand_grid(landscape_name = landscape_names,
                     scale = c('2000', '5000', '10000'),
                     SDM = SDM) |>
      dplyr::mutate(suffix = paste0('_', as.numeric(scale)/1000, 'k'))

    regex2 = ifelse(!is.null(regex), paste0(regex, '_pfld.tif'), '*_pfld.tif')
    purrr::pmap(df2,
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

  } else if (SDM == 'waterbird_win') {
    df = tidyr::expand_grid(landscape_name = landscape_names,
                     scale = c('5000', '10000')) |>
      dplyr::mutate(suffix = paste0('_', as.numeric(scale)/1000, 'k'))

    purrr::pmap(df,
                function(landscape_name, scale, suffix, ...) {
                  python_focal_run(
                    pathin = pathin,
                    landscape_name = landscape_name,
                    SDM = SDM,
                    regex = '*_area.tif',
                    scale = scale,
                    suffix = suffix,
                    fun = 'SUM',
                    dir = dir,
                    python = python,
                    overwrite = overwrite,
                    mask = mask)})

    df2 = tidyr::expand_grid(landscape_name = landscape_names,
                      scale = c('5000', '10000'),
                      SDM = SDM) |>
      dplyr::mutate(suffix = paste0('_', as.numeric(scale)/1000, 'k'))

    purrr::pmap(df2,
                function(landscape_name, scale, suffix, ...) {
                  python_focal_run(
                    pathin = pathin,
                    landscape_name = landscape_name,
                    SDM = SDM,
                    regex = '*_pfld.tif',
                    scale = scale,
                    suffix = suffix,
                    fun = 'MEAN',
                    dir = dir,
                    python = python,
                    overwrite = overwrite,
                    mask = mask)})
  }

}
