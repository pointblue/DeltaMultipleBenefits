#' Rasterize stream channels
#'
#' Convert stream channel line data to rasters for use with "tima" models
#'
#' This function provides support for converting the California Aquatic
#' Resources Inventory (CARI) Streams (lines) data set to a rasterized
#' representation for calculating proportion cover on multiple spatial scales.
#' It uses the same criteria used in the original analysis, lines labeled as
#' 'not shown' or 'Fluvial Subsurface' were excluded. It also provides an option
#' to exclude lines shorter than a minimum length (in meters), which was set to
#' 1 in the original analysis.
#'
#' The CARI data are freely available for download from
#' [SFEI](https://www.sfei.org/data/california-aquatic-resource-inventory-cari-gis-data)
#' and
#' [CDFW](https://filelib.wildlife.ca.gov/Public/BDB/GIS/BIOS/metadata/DS2836.html).
#'
#' By default, this layer will be assumed applicable to the `tima` SDM models
#' and assigned as the "baseline" representation of stream channels unless the
#' `SDM` or `landscape_name` parameters are otherwise specified. This function
#' can be re-run for alternative scenarios (or the resulting files simply copied
#' to the appropriate directory) if stream channel locations and densities will
#' not change in the alternative scenario. Otherwise, the result of this
#' function can be manipulated to represent changes to channel locations under
#' alternative scenarios.
#'
#' @param x object of class sf, or file path to an object than can be read by
#'   `sf`
#' @param template SpatRaster to be used as a template for rasterizing stream
#'   channel data
#' @param min_length Optional; minimum length of stream channel to consider
#' @param SDM The name of intended species distribution model; by default
#'   `"tima"`
#' @param landscape_name The name of the landscape scenario represented; by
#'   default `"baseline"`
#' @param filename The filename of the output raster; by default "CHAN.tif"
#' @param dir Optional string representing directory passed to
#'   [terra::writeRaster()], as (`dir/SDM/landscape_name`). See Details.
#' @param ... Additional arguments passed to [terra::writeRaster()]
#'
#'
#' @returns SpatRaster
#' @export
#'
#'
rasterize_stream_channels = function(x, template, min_length = NULL,
                                     SDM = 'tima', landscape_name = 'baseline',
                                     dir = NULL, filename = 'CHAN.tif', ...) {
  if (is(x, 'character')) {
    x = sf::read_sf(x)
  }
  x = x |>
    dplyr::filter(!.data$clicklabel %in% c('not shown', 'Fluvial Subsurface')) |>
    sf::st_line_merge()

  if (sf::st_crs(template) != sf::st_crs(x)) {
    x = sf::st_transform(crs = terra::crs(template))
  }

  if (!is.null(min_length)) {
    x = x |>
      dplyr::mutate(length = sf::st_length(x) |> as.numeric()) |>
      dplyr::filter(length >= min_length)
  }
  res = terra::rasterize(terra::vect(x), y = template)
  names(res) = 'CHAN'

  if (!is.null(dir)) {
    create_directory(file.path(dir, 'tima', landscape_name))
    terra::writeRaster(res,
                       filename = file.path(dir, 'tima', landscape_name,
                                            filename),
                       ...)
  }
}
