#' Update waterbird predictors: crane roost locations
#'
#' Helper function for estimating impact of landscape changes on known crane
#' roosts, to generate updated estimates of the distance to roost.
#'
#' @details
#' For landscapes that represent a projected change from baseline conditions,
#' evaluates historical crane roosts to determine whether the land cover is
#' projected to become unsuitable (>20% cover with an unsuitable land cover
#' class). Unsuitable roosts are removed, generating files named for
#' `scenario_name` in `pathout`. Use this function prior to using [python_dist]
#' to calculate distance to roost and generate `droost_km.tif`.
#'
#' @param landscape SpatRaster created by [terra::rast()]
#' @param roostpath Filepath to a raster representing the location of
#'   traditional crane roosts.
#' @param pathout Character string; Filepath to directory where output rasters
#'   should be written; passed to [terra::writeRaster()]
#' @param scenario_name Character string; Name of the landscape scenario being
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

update_roosts = function(landscape, roostpath, pathout, scenario_name,
                         overwrite = FALSE) {

  # check how much traditional roosts overlap with incompatible land covers:
  # orchard, vineyard, riparian, woodland, scrub, urban
  roost_overlay = landscape %>%
    subst(from = c(11:19, 60, 70:79, 100:120), to = 1) %>%
    subst(from = c(2:130), to = 0) %>% #everything else
    terra::extract(vect(roostpath))

  # identify polygons to exclude with >20% incompatible landcover
  incompatible = roost_overlay %>% setNames(c('ID', 'landscape')) %>%
    group_by(ID, landscape) %>% count() %>% ungroup() %>%
    group_by(ID) %>% mutate(ncell = sum(n), prop = n/ncell) %>% ungroup() %>%
    filter(landscape == 1 & prop > 0.2) %>% arrange(desc(prop))

  create_directory(file.path(pathout, scenario_name))

  read_sf(roostpath) %>%
    filter(!Roost_ID %in% incompatible$ID) %>%
    vect() %>% rasterize(., landscape) %>%
    writeRaster(file.path(pathout, scenario_name, 'roosts.tif'),
                overwrite = overwrite)
}
