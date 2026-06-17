#' Prepare landscape rasters for focal statistics via Python
#'
#' Prepare for running focal statistics on a landscape raster via Python, to
#' generate inputs for use with species distribution models.
#'
#' This is a wrapper function that calls [create_predictor_stack()] to split a
#' land cover raster into separate layers representing the presence (1) or
#' absence (0) of each land cover class for use in calculating focal statistics.
#' See documentation of that function for information about Warning messages.
#'
#' Optionally, a custom `suffix` can be appended to the layer name and cell
#' values representing land cover presence (1) can be replaced with a different
#' `pixel_value` (e.g., the area of each pixel) as needed before writing the
#' layers to `pathout/SDM/landscape_name`.
#'
#' By providing a `mask`, this function can also use the land cover presence
#' layers as a mask to extract the values of another layer (e.g., surface water
#' data). To distinguish land cover presence from the values extracted from
#' another layer, `suffix` is required to have two values. See examples.
#'
#' @param x SpatRaster
#' @param SDM The name of intended species distribution model, for which `x`
#'   will be reclassified: `"riparian"`, `"waterbird_fall"`, `"waterbird_win"`,
#'   or `"tima"`
#' @param pathout,landscape_name Optional character strings defining the
#'   filepath (`pathout/SDM/landscape_name`) where output rasters should be
#'   written
#' @param suffix Character string; custom suffix appended to layer names
#'   (optional unless `mask` is not `NULL`); see Details.
#' @param mask Optional SpatRaster; see Details
#' @param pixel_value Numeric value to replace cell values with (optional);
#'   default `NULL`
#' @param overwrite Logical; passed to [terra::writeRaster()]; default `FALSE`
#'
#' @return SpatRaster, though primarily used to write layers to file for use
#'   with [python_focal_run()]
#' @seealso [python_focal_run()], [python_focal_finalize()]
#' @export
#'
#' @examples
#' codenums = DeltaMultipleBenefits::key$CODE_NUM
#' r <- terra::rast(matrix(sample(codenums, size = 1000, replace = TRUE), ncol = 100, nrow = 100))
#' watwin = suppressWarnings(classify_landcover(r, SDM = 'waterbird_win', verbose = FALSE))
#' watwin_pred = python_focal_prep(watwin, SDM = 'waterbird_win')
#'
#' # return the area of the pixel where each land cover class is present
#' # (useful for summing over moving windows)
#' watwin_area = python_focal_prep(watwin, SDM = 'waterbird_win', pixel_value = 0.09)
#'
#' # mask another raster (e.g., surface water data) by the presence of each
#' # land cover class:
#' w = watwin # simulate surface water data
#' levels(w) = NULL
#' terra::coltab(w) = NULL
#' terra::values(w) <- sample(c(0,1), size = 10000, replace = TRUE)
#' #pfld = python_focal_prep(watwin, SDM = 'waterbird_win', pixel_value = 0.09, mask = w)
#' #returns error because two suffixes need to be provided
#' pfld = python_focal_prep(watwin, SDM = 'waterbird_win', pixel_value = 0.09, mask = w,
#'                          suffix = c('_area', '_pfld')) # works

python_focal_prep = function(x, SDM,
                             pathout = NULL, landscape_name = NULL,
                             suffix = NULL, mask = NULL, pixel_value = NULL,
                             overwrite = FALSE) {

  if (!is.null(mask) & is.null(suffix)) {
    stop('Provide two suffix values to distinguish unmasked and masked results (e.g., _area and _pfld)')
  }

  # split raster into predictor stack
  presence = create_predictor_stack(x = x, SDM = SDM)

  # optional: if mask is provided (e.g. pfld data), generate layers
  # reflecting the value of the mask layer wherever each land cover is present
  # --> expect two values provided for "suffix" to distinguish them (e.g., _area
  # and _pfld)
  if (!is.null(mask)) {
    # where land cover is absent (presence = 0), change mask to NA (allowing
    # values in mask path to be summarized only for that specific land cover)
    presence_mask = terra::mask(mask, presence, maskvalue = 0, updatevalue = NA)
    names(presence_mask) = paste0(names(presence), suffix[2])

  }

  # finalize & write original unmasked values:
  # optional: replace presence (1) with another value (e.g., pixel area)
  if (!is.null(pixel_value)) {
    presence = terra::subst(presence, from = 1, to = pixel_value)
  }

  # optional: add suffix
  if (!is.null(suffix)) {
    names(presence) = paste0(names(presence), suffix[1])
  }

  if (!is.null(pathout) & !is.null(landscape_name)) {
    create_directory(file.path(pathout, SDM, landscape_name))
    terra::writeRaster(presence,
                       filename = file.path(pathout, SDM, landscape_name,
                                            paste0(names(presence),
                                                   '.tif')),
                       overwrite = overwrite)

    if (!is.null(mask)) {
      terra::writeRaster(presence_mask,
                         filename = file.path(pathout, SDM, landscape_name,
                                              paste0(names(presence_mask),
                                                     '.tif')),
                         overwrite = overwrite)
    }
  }

  if (!is.null(mask)) {
      presence = c(presence, presence_mask)
  }
  return(presence)

}
