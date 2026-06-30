#' Create raster stack representing SDM predictors
#'
#' Prepare for fitting SDMs by transforming a landscape raster into a stack of
#' rasters representing the required predictors.
#'
#' This function is called by [python_focal_prep()] and is not intended to be
#' called directly. Segregates a landscape raster into separate layers
#' representing each land cover class. Also calls on internal datsets to create
#' broader grouping variables as required for the selected SDM. The input raster
#' should already be encoded with the land cover classes listed in the [key].
#'
#' If `classified = FALSE` (the default), this function calls
#' [classify_landcover.SpatRaster()] to reclassify the input raster according to
#' the land cover classifications expected by the selected SDM. (See
#' documentation from that function for information on warning messages.) If
#' `classified = TRUE`, it will be assumed to already be classified correctly,
#' which may be of particular use for the 'riparian' models. See Vignette for
#' details.
#'
#' A warning is given if land cover classes expected by the model are absent
#' from the provided landscape. In that case, if `fill = TRUE` (the default),
#' additional layers will be created with all zero values for each missing land
#' cover class. However, the input landscape should be carefully reviewed to
#' ensure they are truly absent and have not been excluded from the landscape
#' unintentionally. If needed, the resulting layers can be replaced manually
#' before proceeding with [python_focal_run()].
#'
#' @param x SpatRaster; can only have 1 layer
#' @param SDM The name of intended species distribution model: `"riparian"`,
#'   `"waterbird_fall"`, `"waterbird_win"`, or `"tima"`
#' @param classified logical; see Details
#' @param fill logical; see Details
#' @param verbose logical; passed to [classify_landcover.SpatRaster()]
#'
#' @returns SpatRaster with separate layers for each land cover class included
#'   as a predictor in the selected SDM representing the presence (1) and
#'   absence (0)
#' @export
#' @seealso [classify_landcover()]
#'
#' @importFrom utils data
#' @examples
#' r <- terra::rast(matrix(sample(c(11,19,71,72,90), size = 100, replace = TRUE),
#'          ncol = 10, nrow = 10))
#' r = suppressWarnings(create_predictor_stack(r, SDM = 'riparian'))


create_predictor_stack = function(x, SDM, classified = FALSE, fill = TRUE,
                                  verbose = TRUE) {

  # handle main land cover classifications & segregate into layers
  if (classified) {
    x_classified = x
  } else {
    x_classified = classify_landcover(x, SDM = SDM, verbose = verbose)
  }
  layernames = terra::freq(x_classified)$value
  presence = terra::segregate(x_classified, other = 0) |>
    stats::setNames(layernames)

  # add larger grouping predictors necessary for riparian and tima SDMs:
  if (SDM == 'riparian') {
    pred <- DeltaMultipleBenefits::predictors_riparian |>
      dplyr::select(-'NOTES', -'COLOR')
    # drop group vars already represented as unspecified wetland and riparian

    if ('WETLAND' %in% names(presence)) {
      presence = terra::subset(presence, subset = 'WETLAND', negate = TRUE)
    }
    if ('RIPARIAN' %in% names(presence)) {
      presence = terra::subset(presence, subset = 'RIPARIAN', negate = TRUE)
    }

    groupvars = c(
      terra::classify(x,
                      rcl = pred |> dplyr::filter(.data[['RIPARIAN']] == 1) |>
                        dplyr::select(from = 'CODE_NUM') |>
                        dplyr::mutate(to = 1),
                      others = 0),
      terra::classify(x,
                      rcl = pred |> dplyr::filter(.data[['WETLAND']] == 1) |>
                        dplyr::select(from = 'CODE_NUM') |>
                        dplyr::mutate(to = 1),
                      others = 0)
    )
    names(groupvars) = c('RIPARIAN', 'WETLAND')
    presence = c(presence, groupvars)

  } else if (SDM == 'tima') {
    pred <- DeltaMultipleBenefits::predictors_tima |>
      dplyr::select(-'COLOR')
    # drop partial layers already in presence
    if ('NWET' %in% names(presence)) {
      presence = terra::subset(presence, subset = 'NWET', negate = TRUE)
    }
    if ('TWET' %in% names(presence)) {
      presence = terra::subset(presence, subset = 'TWET', negate = TRUE)
    }
    if ('WETL' %in% names(presence)) {
      presence = terra::subset(presence, subset = 'WETL', negate = TRUE)
    }
    if ('RFOR' %in% names(presence)) {
      presence = terra::subset(presence, subset = 'RFOR', negate = TRUE)
    }
    if ('RSCR' %in% names(presence)) {
      presence = terra::subset(presence, subset = 'RSCR', negate = TRUE)
    }
    # reclassify from original input landcover
    groupvars = c(
      terra::classify(x,
                      rcl = pred |> dplyr::filter(.data[['NWET']] == 1) |>
                        dplyr::select(from = 'CODE_NUM') |>
                        dplyr::mutate(to = 1),
                      others = 0),
      terra::classify(x,
                      rcl = pred |> dplyr::filter(.data[['TWET']] == 1) |>
                        dplyr::select(from = 'CODE_NUM') |>
                        dplyr::mutate(to = 1),
                      others = 0),
      terra::classify(x,
                      rcl = pred |> dplyr::filter(.data[['WETL']] == 1) |>
                        dplyr::select(from = 'CODE_NUM') |>
                        dplyr::mutate(to = 1),
                      others = 0),
      terra::classify(x,
                      rcl = pred |> dplyr::filter(.data[['RFOR']] == 1) |>
                        dplyr::select(from = 'CODE_NUM') |>
                        dplyr::mutate(to = 1), others = 0),
      terra::classify(x,
                      rcl = pred |> dplyr::filter(.data[['RSCR']] == 1) |>
                        dplyr::select(from = 'CODE_NUM') |>
                        dplyr::mutate(to = 1),
                      others = 0))
    names(groupvars) = c('NWET', 'TWET', 'WETL', 'RFOR', 'RSCR')
    presence = c(presence, groupvars)

  } else if (SDM == 'waterbird_fall') {
    pred <- DeltaMultipleBenefits::predictors_waterbird_fall |>
      dplyr::select(-'COLOR')
  } else if (SDM == 'waterbird_win') {
    pred <- DeltaMultipleBenefits::predictors_waterbird_win |>
      dplyr::select(-'COLOR')
  }

  # check that all unique predictors are accounted for and add rasters of all
  # 0 values if necessary
  pred_unique = unique(stats::na.omit(pred$PREDICTOR_NAME))
  if(SDM %in% c('riparian', 'tima')) { # additional grouping vars
    pred_unique = c(pred_unique,
                    pred |> dplyr::select(5:dplyr::last_col()) |> names())
  }

  if (!all(pred_unique %in% names(presence))) {
    missing = pred_unique[!pred_unique %in% names(presence)]
    cat(missing)
    warning(
      strwrap(
        prefix = " ", initial = "",
        "Extreme Caution Advised. Land cover classes are missing from the
        input raster but are expected by the selected SDM. Check input raster
        for errors."))
    if (fill) {
      message(
        strwrap(
          "Because fill = TRUE, creating missing rasters with all zero values,
          but confirm they are truly absent from the landscape."))
      rnew = x
      levels(rnew) = NULL
      rnew[rnew > 0] <- 0 # raster with all zero values
      addme = list(rnew) |> rep(length(missing)) |> terra::rast()
      names(addme) = missing
      presence = c(presence, addme)
    }
  }

  return(presence)
}
