#' Update waterbird predictors: crane roost locations
#'
#' Helper function for estimating impact of landscape changes on the locations
#' of known crane roosts.
#'
#' @details For landscapes that represent a projected change from baseline
#'   conditions, this function facilitates evaluating historical crane roost
#'   polygons to determine whether the land cover overlaying them in raster `x`
#'   is now unsuitable, based on exceeding a threshold `proportion` covered by
#'   an unsuitable land cover class. Unsuitable roost polygons are removed and
#'   the rest rasterized to match `x`. Use this function prior to using
#'   [python_dist()] to calculate distance to roost and generate updated
#'   versions of `droost_km.tif` for each scenario.
#'
#'   The default values for `unsuitable` include the original encodings for
#'   orchard and vineyard classes (10-19), urban (60), riparian (70-79,
#'   170-187), and woodland and scrub (100-120), and the default threshold value
#'   for `proportion` is 0.2.
#'
#' @param x SpatRaster
#' @param unsuitable numerical vector representing the land cover
#'   classifications in `x` that are incompatible with crane roosts
#' @param proportion numerical threshold at which cover by an unsuitable land
#'   cover class makes a roost polygon unsuitable; see Details
#' @param roosts SpatVector or character string giving the filepath to polygons
#'   representing the location of traditional crane roosts
#' @param filename Optional character string passed to [terra::writeRaster()]
#' @param ... Additional arguments passed to [terra::writeRaster()]
#'
#' @seealso [update_covertype()], [update_pwater()]
#' @export
#'
#' @examples
#' # See vignette

update_roosts = function(x, unsuitable = c(10:19, 60, 70:79, 100:120, 170:187),
                         proportion = 0.2, roosts, filename = NULL, ...) {

  if (is(roosts, 'character')) {
    roosts = terra::vect(roosts)
  } else if (!is(roosts, 'SpatVector')) {
    stop('function expects "roosts" to be either a character string or a SpatVector')
  }

  if (terra::crs(x) != terra::crs(roosts)) {
    roosts = terra::project(roosts, x)
  }

  if (!'Roost_ID' %in% names(roosts)) {
    roosts$Roost_ID = c(1:nrow(roosts))
  }
  # check how much traditional roosts overlap with incompatible land covers:
  # orchard, vineyard, riparian, woodland, scrub, urban
  levels(x) <- NULL
  roost_overlay = x |>
    terra::subst(from = unsuitable, to = 1, others = 0) |>
    terra::extract(roosts)

  # identify polygons to exclude with >20% incompatible landcover
  incompatible = roost_overlay |>
    stats::setNames(c('ID', 'landscape')) |>
    dplyr::group_by(.data$ID, .data$landscape) |>
    dplyr::count() |>
    dplyr::ungroup() |>
    dplyr::group_by(.data$ID) |>
    dplyr::mutate(ncell = sum(.data$n), prop = .data$n/.data$ncell) |>
    dplyr::ungroup() |>
    dplyr::filter(.data$landscape == 1 & .data$prop > proportion) |>
    dplyr::arrange(dplyr::desc(.data$prop))

  roosts_update = roosts[-which(roosts$Roost_ID %in% incompatible$ID)]
  roosts_raster = terra::rasterize(roosts_update, x)

  if (!is.null(filename)) {
    terra::writeRaster(roosts_raster, filename, ...)
  }
  return(roosts_raster)
}
