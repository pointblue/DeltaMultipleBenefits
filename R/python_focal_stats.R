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
#'   function calls [python_focal_run()] to calculate focal statistics required
#'   for each species distribution model, on the appropriate spatial scales and
#'   with the required summary statistics. The `regex` argument provides options
#'   for processing only a subset of the rasters in the directory. If focal
#'   statistics have already been created and need to be re-run, previous
#'   versions need to be manually deleted or else the output `dir` changed;
#'   there is currently no option to overwrite.
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
#'
#' @return Nothing returned to R environment. Writes rasters to `pathout` for
#'   each land cover class.
#' @seealso [python_focal_prep()], [python_focal_finalize()]
#' @export
#'
#' @examples
#' # See vignette

python_focal_stats = function(SDM, pathin, dir, landscape_names,
                              regex = NULL, python = NULL) {
  if (SDM == 'tima') {
    # automatically choose the correct scales and function and put the purrr in here
    df = tidyr::expand_grid(landscape_name = landscape_names,
                            scale = c('100', '2000')) |>
      dplyr::mutate(suffix = scale,
                    SDM = SDM,
                    pathin = pathin,
                    dir = dir,
                    regex = regex,
                    fun = 'MEAN',
                    python = python)

    purrr::pmap(df, python_focal_run)

  } else if (SDM == 'riparian') {
    # automatically choose the correct scales and function and put the purrr in here
    df = tidyr::expand_grid(landscape_name = landscape_names,
                            scale = c('50', '2000')) |>
      dplyr::mutate(suffix = paste0('_', scale),
                    SDM = SDM,
                    pathin = pathin,
                    dir = dir,
                    regex = regex,
                    fun = 'MEAN',
                    python = python)

    purrr::pmap(df, python_focal_run)

  } else if (SDM == 'waterbird_fall') {

    df = expand_grid(landscape_name = landscape_names,
                     scale = c('2000', '5000', '10000')) |>
      dplyr::mutate(suffix = paste0('_', as.numeric(scale)/1000, 'k'),
                    SDM = SDM,
                    pathin = pathin,
                    dir = dir,
                    regex = '*_area.tif',
                    fun = 'SUM',
                    python = python)

    purrr::pmap(df, python_focal_run)

    df2 = expand_grid(landscape_name = landscape_names,
                     scale = c('2000', '5000', '10000'),
                     SDM = SDM) |>
      dplyr::mutate(suffix = paste0('_', as.numeric(scale)/1000, 'k'),
                    SDM = SDM,
                    pathin = pathin,
                    dir = dir,
                    regex = '*_pfld.tif',
                    fun = 'MEAN',
                    python = python)

    purrr::pmap(df2, python_focal_run)


  } else if (SDM == 'waterbird_win') {
    df = expand_grid(landscape_name = landscape_names,
                     scale = c('5000', '10000')) |>
      dplyr::mutate(suffix = paste0('_', as.numeric(scale)/1000, 'k'),
                    SDM = SDM,
                    pathin = pathin,
                    dir = dir,
                    regex = '*_area.tif',
                    fun = 'SUM',
                    python = python)

    purrr::pmap(df, python_focal_run)

    df2 = expand_grid(landscape_name = landscape_names,
                      scale = c('5000', '10000'),
                      SDM = SDM) |>
      dplyr::mutate(suffix = paste0('_', as.numeric(scale)/1000, 'k'),
                    SDM = SDM,
                    pathin = pathin,
                    dir = dir,
                    regex = '*_pfld.tif',
                    fun = 'MEAN',
                    python = python)

    purrr::pmap(df2, python_focal_run)
  }

}
