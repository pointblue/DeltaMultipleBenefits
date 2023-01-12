#' Prepare landscape rasters for focal statistics via Python
#'
#' Prepare for running focal statistics on a landscape raster via Python, to
#' generate inputs for use with species distribution models.
#'
#' Splits landscape raster into separate layers representing the presence (1) or
#' absence (0) of each land cover class, then regroups and renames them into the
#' land cover classes used in by the intended species distribution model
#' (`SDM`), with an optional custom `suffix` appended to the layer name. Cell
#' values representing land cover presence (1) can also optionally be replaced
#' with a different `pixel_value` (e.g., the area of each pixel). If `pathout`
#' is provided (recommended), rasters are written to `pathout`, in a
#' subdirectory corresponding to the name of the `SDM` provided, with the land
#' cover class names used by the intended species distribution model. File names
#' can optionally have a custom `suffix` appended.
#'
#' By providing a `mask`, this function can also optionally use the land cover
#' presence layers as a mask to extract the values of another layer (e.g.,
#' surface water data). To distinguish these layers, `suffix` is required to
#' have two values. See examples.
#'
#' @param landscape SpatRaster created by [terra::rast()]
#' @param SDM The name of intended species distribution model, for which
#'   `landscape` will be reclassified, and corresponding name of the
#'   subdirectory within `pathout/scenario_name` where results will be written:
#'   `"riparian"`, `"waterbird_fall"`, or `"waterbird_win"`
#' @param pathout Character string; Optional filepath to directory where output
#'   rasters should be written; passed to [terra::writeRaster()]
#' @param landscape_name Character string; Name of the landscape scenario being
#'   evaluated, corresponding to the subdirectory in `pathout` where results
#'   will be written.
#' @param suffix Character string; custom suffix appended to layer names
#'   (optional unless `maskpath` is not `NULL`)
#' @param mask Optional SpatRaster; see Details
#' @param pixel_value Numeric value to replace cell values with (optional);
#'   default `NULL`
#' @param overwrite Logical; passed to [terra::writeRaster()]; default `FALSE`
#'
#' @return Nothing returned to R environment. Writes rasters to `pathout` for
#'   each land cover class.
#' @seealso [python_focal_run()], [python_focal_finalize()]
#' @export
#'
#' @examples
#' #f <- system.file("ex/elev.tif", package="terra")
#' #r <- terra::rast(f) # add an example
#' #python_prep(landscape = r, SDM = 'riparian', pathout = 'example')
#'
#' #try(python_prep(landscape = r, SDM = 'waterbird_win', pathout = 'example',
#' #pixel_value = 0.09, maskpath = system.file('ex/elev.tif', package = 'terra')))
#' ## suffix is required if maskpath is not `NULL`
#'
#' #python_prep(landscape = r, SDM = 'waterbird_win', pathout = 'example',
#' #pixel_value = 0.09, maskpath = system.file('ex/elev.tif', package = 'terra'),
#' #suffix = c('_area', '_elev'))

python_focal_prep = function(landscape, SDM, pathout, landscape_name,
                             suffix = NULL, mask = NULL, pixel_value = NULL,
                             overwrite = FALSE) {

  if (!is.null(mask) & is.null(suffix)) {
    stop('Provide two suffix values to distinguish unmasked and masked results (e.g., _area and _pfld)')
  }

  # split layer by land cover classes to represent presence/absence
  layernames = terra::freq(landscape) %>% dplyr::pull(label)
  presence = terra::segregate(landscape, other = 0) %>%
    setNames(layernames)

  # reclassify according to riparian and waterbird model inputs
  presence_reclass = reclassify_landcover(presence, SDM = SDM)

  create_directory(file.path(pathout, landscape_name, SDM))

  # optional: if mask is provided (e.g. pfld data), generate layers
  # reflecting the value of the mask layer wherever each land cover is present
  # --> expect two values provided for "suffix" to distinguish them (e.g., _area
  # and _pfld)
  if (!is.null(mask)) {
    # where presence_reclass is 0 (land cover not present), change maskpath to
    # NA (allowing values in mask path to be summarized only for that specific
    # land cover)
    newstack_mask = terra::mask(mask, presence_reclass, maskvalue = 0,
                                updatevalue = NA)
    names(newstack_mask) = paste0(names(presence_reclass), suffix[2])
    terra::writeRaster(newstack_mask,
                       filename = file.path(pathout, landscape_name, SDM,
                                            paste0(names(newstack_mask),
                                                   '.tif')),
                       overwrite = overwrite)
  }

  # finalize original unmasked values:
  # optional: replace presence (1) with another value (e.g., pixel area)
  if (!is.null(pixel_value)) {
    presence_reclass = terra::subst(presence_reclass,
                                    from = 1, to = pixel_value)
  }

  # optional: add suffix
  if (!is.null(suffix)) {
    names(presence_reclass) = paste0(names(presence_reclass), suffix[1])
  }

  terra::writeRaster(presence_reclass,
                     filename = file.path(pathout, landscape_name, SDM,
                                          paste0(names(presence_reclass),
                                                 '.tif')),
                     overwrite = overwrite)
}
