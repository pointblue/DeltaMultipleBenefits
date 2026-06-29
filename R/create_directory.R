#' create_directory
#'
#' Helper function called within other functions to check whether the intended
#' output directory exists, and if not, create it. Particularly useful when many
#' files are generated programmatically with an expected structure.
#'
#' @param filepath character string representing one or more file paths
#'
#' @return Prints either "Creating directory: filepath" if directory did not
#'   already exist, or "Writing to directory: filepath" if it did already exist.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' create_directory(filepath = "Desktop/example")
#'
#' # will create nested directories as needed
#' create_directory(filepath = "Desktop/example/test")
#' }
#'

create_directory = function(filepath) {
  if (any(dir.exists(filepath))) {
    f = filepath[dir.exists(filepath)]
    purrr::walk(f, ~cat('Writing to directory:', .x, '\n'))
  }
  if (any(!dir.exists(filepath))) {
    f = filepath[!dir.exists(filepath)] |> normalizePath()
    purrr::walk(f, ~dir.create(.x, recursive = TRUE))
    purrr::walk(f, ~cat('Creating directory:', .x, '\n'))
  }
}
