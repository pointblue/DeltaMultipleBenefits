#' Create raster stack representing SDM predictors
#'
#' Prepare for fitting SDMs by transforming a landscape raster into a stack of
#' rasters representing the required predictors.
#'
#' This function is called by [python_focal_prep()] and is not intended to be
#' called directly. Segregates a landscape raster into separate layers
#' representing each land cover class. Also calls on internal datsets to create
#' broader grouping variables as required for the selected SDM. The input raster
#' should already be classified according to the land cover classifications
#' expected by the selected SDM. See [classify_landcover.SpatRaster()].
#'
#' A warning is given if land cover classes expected by the model are absent
#' from the landscape, and rasters with all zero values are created to represent
#' them. However, the input raster should be carefully reviewed to ensure they
#' are truly absent and have not bee excluded from the landscape
#' unintentionally. If needed, the resulting layers can be replaced manually
#' before proceeding with [python_focal_run()].
#'
#' @param x SpatRaster
#' @param SDM The name of intended species distribution model: `"riparian"`,
#'   `"waterbird_fall"`, `"waterbird_win"`, or `"tima"`
#'
#' @return SpatRaster with separate layers for each land cover class included as
#'   a predictor in the selected SDM representing the presence (1) and absence
#'   (0)
#' @export
#' @seealso [classify_landcover()]
#'
#' @importFrom utils data
#' @examples
#' r <- terra::rast(matrix(sample(c(11,19,71,72,90), size = 100, replace = TRUE),
#'          ncol = 10, nrow = 10))
#' r = suppressWarnings(create_predictor_stack(r, SDM = 'riparian'))


create_predictor_stack = function(x, SDM) {

  # segregate
  layernames = terra::freq(x)$value
  presence = terra::segregate(x, other = 0) |> stats::setNames(layernames)

  # add larger grouping predictors necessary for riparian and tima SDMs:
  if (SDM == 'riparian') {
    pred <- DeltaMultipleBenefits::predictors_riparian |>
      dplyr::select(-.data$NOTES, -.data$COLOR)
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
      dplyr::select(-.data$COLOR)
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
      dplyr::select(-.data$COLOR)
  } else if (SDM == 'waterbird_win') {
    pred <- DeltaMultipleBenefits::predictors_waterbird_win |>
      dplyr::select(-.data$COLOR)
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
        for errors. Creating rasters with all zero values, but confirm they are
        truly absent from the landscape."))
    rnew = x
    rnew[!is.na(rnew)] <- 0 # raster with all zero values
    addme = list(rnew) |> rep(length(missing)) |> terra::rast()
    names(addme) = missing
    presence = c(presence, addme)
  }

  return(presence)
}
