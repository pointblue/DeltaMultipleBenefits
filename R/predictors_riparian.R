#' Predictors used in distribution models for riparian focal species
#'
#' Predictor names and how they crosswalk to the major land cover classes and
#' subclasses listed in the vegetation `key`. Land cover classes
#' or subclasses with `NA` values in `PREDICTOR_NAME` but `999` in
#' `PREDICTOR_NUM` indicate that they are included as part of a larger grouping
#' predictor, as flagged in the `RIPARIAN` or `WETLAND` columns.
#'
#' This data set is primarily for use in the [reclassify_landcover()] function.
#'
#' @format ## `key` A data frame with 81 rows and 6 columns:
#' \describe{
#'   \item{CODE_NUM}{Numeric value used to encode rasters, matching `key`}
#'   \item{CODE_NAME}{Text string joining major land cover classes to subclasses with a '_', matching `key`}
#'   \item{PREDICTOR_NAME}{Corresponding predictor name in tidal marsh bird distribution models}
#'   \item{PREDICTOR_NUM}{Corresponding numeric value used to reclassify land cover rasters for use with tidal marsh bird distribution models}
#'   \item{RIPARIAN}{Additional predictor grouping riparian subclasses together; 1 indicates the included subclasses}
#'   \item{WETLAND}{Additional predictor grouping wetland subclasses together; 1 indicates the included subclasses}
#' }
#' @source Dybala et al. 2023 (https://doi.org/10.15447/sfews.2023v21iss3art4)
"predictors_riparian"
