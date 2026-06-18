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
#' If `suffix` is provided, it is appended to the layer name. If `pixel_value`
#' is provided, values representing land cover presence (1) are replaced with
#' this value (e.g., the area of each pixel).
#'
#' If `subset` is provided, either as a SpatRaster or a filepath to a raster,
#' the `subset` layer is masked by the presence of each land cover layer, e.g.
#' to extract from surface water data the extent of surface water within each
#' land cover class. In this case, both the land cover presence data and the
#' subset data are returned; to distinguish between these results, `suffix` is
#' required to have two values. The first is appended to the land cover presence
#' data and the second is appended to the subset data. See examples.
#'
#' All output can be optionally written to `dir/SDM/landscape_name` where
#' `landscape_name` is taken from the names of the input raster(s) `x`.
#'
#' @param x SpatRaster with layer names corresponding to `landscape_name`
#' @param SDM The name of intended species distribution model, for which `x`
#'   will be reclassified: `"riparian"`, `"waterbird_fall"`, `"waterbird_win"`,
#'   or `"tima"`
#' @param fill logical; see Details
#' @param suffix Character string; custom suffix appended to layer names
#'   (optional unless `mask` is not `NULL`); see Details.
#' @param subset Optional SpatRaster or string representing filepath to a
#'   raster. See Details.
#' @param pixel_value Optional numeric value to use in place of 1 where land
#'   covers are present; default `NULL`
#' @param dir Optional string representing directory passed to
#'   [terra::writeRaster()], as (`dir/SDM/landscape_name`). See Details.
#' @param overwrite logical. If `TRUE`, output is overwritten
#' @param ... additional arguments passed to [terra::writeRaster()]
#'
#' @returns SpatRaster, though primarily used to write layers to file for use
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

python_focal_prep = function(x, SDM, fill = TRUE, suffix = NULL,
                             pixel_value = NULL, subset = NULL, dir = NULL,
                             overwrite = FALSE, ...) {

  if (!is.null(subset) & is.null(suffix)) {
    stop('Provide two suffix values to distinguish unmasked and masked results (e.g., _area and _pfld)')
  }

  # split raster into predictor stack; if multiple layers, repeat for each;
  # keep as a list to allow each set to remain separate
  presence = purrr::map(
    c(1:terra::nlyr(x)),
    ~create_predictor_stack(x = x[[.x]], SDM = SDM, fill = fill))
  landscape_names = names(x)
  names(presence) = landscape_names

  # optional: if subset is provided (e.g. pfld data), generate layers
  # reflecting the value of the subset layer wherever each land cover is present
  # --> expect two values provided for "suffix" to distinguish them (e.g., _area
  # and _pfld)
  if (!is.null(subset)) {
    if (is(subset, 'character')) {
      subset = terra::rast(subset)
    } else if (!is(subset, 'SpatRaster')) {
      stop('function expects "subset" to be either a character string or a SpatRaster')
    }

    # where land cover is absent (presence = 0 or NA), change masklayer to NA
    # (allowing values in mask to be summarized only for that specific land
    # cover)
    presence_mask = mask_predictors(lc = presence, masklayer = subset, suffix)
    names(presence_mask) = landscape_names
  }

  # optional: replace presence (1) with another value (e.g., pixel area)
  if (!is.null(pixel_value)) {
    presence = purrr::map(
      presence,
      ~terra::classify(.x, rcl = data.frame(from = 1, to = pixel_value) |> as.matrix())
    )
  }

  # optional: add suffix
  if (!is.null(suffix)) {
    presence = purrr::map(
      presence,
      function(x) {names(x) = paste0(names(x), suffix[1])}
    )
  }

  if (!is.null(dir) & !is.null(landscape_names)) {
    for (i in c(1:length(landscape_names))) {
      create_directory(file.path(dir, SDM, landscape_names[i]))
      terra::writeRaster(presence[[i]],
                         filename = file.path(dir, SDM, landscape_names[i],
                                              paste0(names(presence[[i]]),
                                                     '.tif')),
                         overwrite = overwrite, ...)
      if (!is.null(subset)) {
        # also write out masked versions
        terra::writeRaster(presence_mask[[i]],
                           filename = file.path(dir, SDM, landscape_names[i],
                                                paste0(names(presence_mask[[i]]),
                                                       '.tif')),
                           overwrite = overwrite, ...)
      }
    }
  }

  if (!is.null(subset)) {
    presence = list('presence' = presence, 'presence_mask' = presence_mask)
  }
  return(presence)

}

mask_predictors = function(lc, masklayer, suffix) {
  purrr::map(names(lc),
             function(x) {
               r = terra::mask(masklayer, lc[[x]], maskvalues = c(0, NA))
               names(r) = paste0(names(lc[[x]]), suffix[2])
               return(r)
             })
}
