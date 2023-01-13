#' Land cover classification scheme for the `DeltaMultipleBenefits` framework
#'
#' Major land cover classes and subclasses designed to work with the existing
#' metrics and species distribution models within the `DeltaMultipleBenefits`
#' framework. It includes both natural and agricultural land cover classes, and
#' is organized hierarchically into major land cover classes and subclasses.
#' Also included are default values for labels and color coding used in maps.
#'
#' @format ## `key` A data frame with 41 rows and 7 columns:
#' \describe{
#'   \item{CODE_BASELINE}{Numeric value used to encode rasters}
#'   \item{CODE_NAME}{Text string joining major land cover classes to subclasses with a '_'}
#'   \item{CLASS}{Major land cover class grouping}
#'   \item{SUBCLASS}{Land cover subclass}
#'   \item{DETAIL}{Further land cover detail}
#'   \item{LABEL}{Default value for labels used in maps and plots}
#'   \item{COLOR}{Default hex color code used in maps}
#' }
#' @source Dybala et al. In prep (URL to be determined)
"key"
