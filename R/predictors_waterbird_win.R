#' Predictors used in distribution models for waterbird groups during the winter
#' season
#'
#' Predictor names and how they crosswalk to the major land cover classes and
#' subclasses listed in the vegetation `key`. This crosswalk is nearly identical
#' to `predictors_waterbirds_fall` except that in the winter, agricultural
#' fields specified as "winter wheat" are separated from other grains.
#'
#' Land cover classes or subclasses with `NA` values in `PREDICTOR_NAME` and
#' `PREDICTOR_NUM` are excluded from these models.
#'
#' This data set is primarily for use in the [reclassify_landcover()] function.
#' See [classify_LSPT()] for an alternative approach to classify the original
#' land cover polygons.
#'
#' @format ## `predictors_waterbird_win` A data frame with 81 rows and 4
#'   columns:
#' \describe{
#'   \item{CODE_NUM}{Numeric value used to encode rasters, matching `key`}
#'   \item{CODE_NAME}{Text string joining major land cover classes to subclasses with a '_', matching `key`}
#'   \item{PREDICTOR_NAME}{Corresponding predictor name in tidal marsh bird distribution models}
#'   \item{PREDICTOR_NUM}{Corresponding numeric value used to reclassify land cover rasters for use with tidal marsh bird distribution models}
#' }
#' @source Dybala et al. 2023 (https://doi.org/10.15447/sfews.2023v21iss3art4)
"predictors_waterbird_win"
