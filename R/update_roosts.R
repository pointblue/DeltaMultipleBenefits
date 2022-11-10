#' Update waterbird predictors: crane roost locations
#'
#' Helper function for estimating impact of landscape changes on known crane
#' roosts, to generate updated estimates of the distance to roost for use with
#' waterbird distribution models.
#'
#' @details For landscapes that represent a projected change from baseline
#'   conditions, this function facilitates evaluating historical crane roosts to
#'   determine whether the land cover overlaying them in `landscape` is
#'   projected to become unsuitable (>20% cover with an unsuitable land cover
#'   class, including orchard, vineyard, riparian, woodland, scrub, urban).
#'   Unsuitable roost polygons are removed, and updated roost maps are
#'   generated, named as `scenario_name` in `pathout`. Use this function prior
#'   to using [python_dist] to calculate distance to roost and generate updated
#'   versions of `droost_km.tif` for each scenario.
#'
#' @param landscape SpatRaster created by [terra::rast()]
#' @param roostpath Filepath to a raster representing the location of
#'   traditional crane roosts.
#' @param pathout Character string; Filepath to directory where output rasters
#'   should be written; passed to [terra::writeRaster()]
#' @param landscape_name Character string; Name of the landscape being
#'   evaluated, corresponding to the directory in `pathout` where results will
#'   be written.
#' @param overwrite Logical; passed to [terra::writeRaster()]; default `FALSE`
#'
#' @return Nothing; all files written to `pathout`
#' @seealso [update_covertype()], [update_pwater()]
#' @export
#'
#' @examples
#' # See vignette

update_roosts = function(landscape, roostpath, pathout, landscape_name,
                         overwrite = FALSE) {

  # check how much traditional roosts overlap with incompatible land covers:
  # orchard, vineyard, riparian, woodland, scrub, urban
  roost_overlay = landscape %>%
    terra::subst(from = c(11:19, 60, 70:79, 100:120), to = 1) %>%
    terra::subst(from = c(2:130), to = 0) %>% #everything else
    terra::extract(terra::vect(roostpath))

  # identify polygons to exclude with >20% incompatible landcover
  incompatible = roost_overlay %>% setNames(c('ID', 'landscape')) %>%
    dplyr::group_by(ID, landscape) %>%
    dplyr::count() %>%
    dplyr::ungroup() %>%
    dplyr::group_by(ID) %>%
    dplyr::mutate(ncell = sum(n), prop = n/ncell) %>%
    dplyr::ungroup() %>%
    dplyr::filter(landscape == 1 & prop > 0.2) %>%
    dplyr::arrange(dplyr::desc(prop))

  create_directory(file.path(pathout, landscape_name))

  sf::read_sf(roostpath) %>%
    dplyr::filter(!Roost_ID %in% incompatible$ID) %>%
    terra::vect() %>% terra::rasterize(., landscape) %>%
    terra::writeRaster(file.path(pathout, landscape_name, 'roosts.tif'),
                       overwrite = overwrite)
}
