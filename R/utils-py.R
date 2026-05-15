#' Python helpers
#'
#' Internal helpers that perform one-time Python initialization and provide
#' per-module lazy loaders. These functions are internal and should not be
#' called directly by users.
#'
#' @keywords internal
#' @importFrom reticulate use_python
#' @importFrom reticulate import
#' @importFrom reticulate source_python


.py_state <- new.env(parent = emptyenv())

.py_shared_init <- local({
  done <- FALSE
  function(python = NULL) {
    if (!done) {
      if (!is.null(python)) reticulate::use_python(python, required = TRUE)
      .py_state$arcpy <- reticulate::import("arcpy")
      .py_state$arcpy$CheckOutExtension("Spatial")
      done <<- TRUE
    }
    .py_state$arcpy
  }
})

# per-module lazy loader factory
load_py_script <- function(py_name) {
  path <- system.file("python", paste0(py_name, ".py"),
                      package = "DeltaMultipleBenefits")
  env <- new.env(parent = emptyenv())
  reticulate::source_python(path, convert = FALSE, envir = env)
  env
}

