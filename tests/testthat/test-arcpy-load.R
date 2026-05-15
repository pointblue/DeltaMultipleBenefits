testthat::test_that("arcpy can load", {
  testthat::skip_if_not_installed("reticulate")
  testthat::skip_on_cran()

  init <- getFromNamespace(".py_shared_init", "DeltaMultipleBenefits")
  pythonpath = 'C:/Program Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3/python.exe'
  init(python = pythonpath)
  testthat::expect_equal(pythonpath, reticulate::py_config()$python)
})

