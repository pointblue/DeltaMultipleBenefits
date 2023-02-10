#' Update waterbird predictors: crane roost locations
#'
#' Helper function for estimating impact of landscape changes on the locations
#' of known crane roosts.
#'
#' @details For landscapes that represent a projected change from baseline
#'   conditions, this function facilitates evaluating historical crane roosts to
#'   determine whether the land cover overlaying them in `landscape` is
#'   projected to become unsuitable, based on exceeding a threshold `proportion`
#'   covered by an unsuitable land cover class. Unsuitable roost polygons are
#'   removed, and updated roost maps are generated, named as `scenario_name` in
#'   `pathout`. Use this function prior to using [python_dist] to calculate
#'   distance to roost and generate updated versions of `droost_km.tif` for each
#'   scenario.
#'
#'   The default values for `unsuitable` include the original encodings for
#'   orchard, vineyard, riparian, woodland, scrub, and urban land cover classes,
#'   and the default threshold value for `proportion` is 0.2. Alternate values
#'   can be provided as desired.
#'
#' @param landscape SpatRaster created by [terra::rast()]
#' @param unsuitable optional vector of numerical values representing the land
#'   cover classifications that should be considered incompatible with crane
#'   roosts; default values for the original land cover encoding include orchard
#'   & vineyard classes (11-19), urban (60), riparian classes (70-79), and
#'   woodland & scrub classes (100-120)
#' @param proportion numerical value for the proportion cover by an unsuitable
#'   land cover class at which the roost should be considered unsuitable; see
#'   Details
#' @param roosts SpatVector created by [terra::vect()] or character string
#'   giving the filepath to polygons representing the location of traditional
#'   crane roosts; expects attribute called "Roost_ID"
#' @param pathout,landscape_name Character strings defining the filepath
#'   (`pathout/landscape_name`) where updated roost location rasters should be
#'   written
#' @param overwrite Logical; passed to [terra::writeRaster()]; default `FALSE`
#'
#' @return Nothing; all files written to `pathout/landscape_name`
#' @seealso [update_covertype()], [update_pwater()]
#' @export
#'
#' @examples
#' # See vignette

update_roosts = function(landscape, unsuitable = c(11:19, 60, 70:79, 100:120),
                         proportion = 0.2, roosts, pathout, landscape_name,
                         overwrite = FALSE) {

  if (is(roosts, 'character')) {
    roosts = terra::vect(roosts)
  } else if (!is(roosts, 'SpatVector')) {
    stop('function expects "roosts" to be either a character string or a SpatVector')
  }

  if (terra::crs(landscape) != terra::crs(roosts)) {
    roosts = terra::project(roosts, landscape)
  }

  # check how much traditional roosts overlap with incompatible land covers:
  # orchard, vineyard, riparian, woodland, scrub, urban
  levels(landscape) <- NULL
  roost_overlay = landscape %>%
    terra::subst(from = unsuitable, to = 1) %>%
    terra::subst(from = c(2:999), to = 0) %>% #everything else
    terra::extract(roosts)

  # identify polygons to exclude with >20% incompatible landcover
  incompatible = roost_overlay %>% setNames(c('ID', 'landscape')) %>%
    dplyr::group_by(.data$ID, .data$landscape) %>%
    dplyr::count() %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$ID) %>%
    dplyr::mutate(ncell = sum(.data$n), prop = .data$n/.data$ncell) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$landscape == 1 & .data$prop > proportion) %>%
    dplyr::arrange(dplyr::desc(.data$prop))

  roosts_update = roosts[-which(roosts$Roost_ID %in% incompatible$ID)]
  roosts_raster = terra::rasterize(roosts_update, landscape)

  create_directory(file.path(pathout, landscape_name))
  terra::writeRaster(roosts_raster,
                     file.path(pathout, landscape_name, 'roosts.tif'),
                     overwrite = overwrite)
}
