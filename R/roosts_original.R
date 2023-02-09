#' Traditional nighttime crane roost locations
#'
#' Original crane roost locations used in generating the "distance to roost"
#' predictor variable used in the crane distribution models within the
#' `DeltaMultipleBenefits` framework. The assumed future locations of roosts
#' under scenarios of landscape change may require modification if land covers
#' become incompatible (see [DeltaMultipleBenefits::update_roosts()])
#'
#' @format ## `roosts_original` A simple features collection with 94 geometries 1
#'   attribute field:
#' \describe{
#'   \item{Roost_ID}{Numeric value used to identify distinct roosts}
#' }
#'
"roosts_original"
