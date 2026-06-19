
# test ability to return a per-pixel value
test_that('pixel values are correctly applied', {
  codenums = DeltaMultipleBenefits::key$CODE_NUM
  r <- terra::rast(matrix(sample(codenums, size = 1000, replace = TRUE),
                          ncol = 100, nrow = 100))
  r2 = suppressWarnings(classify_landcover(r, SDM = 'waterbird_win', verbose = FALSE))
  area = suppressWarnings(python_focal_prep(r2, SDM = 'waterbird_win', pixel_value = 0.09))
  expect_true(all(unique(terra::values(area[[1]]) %in% c(0.00, 0.09, NaN))))
})

# TEST ERRORS: (same as for create_predictor stack)
test_that('warnings given', {
  codenums = unique(na.omit(DeltaMultipleBenefits::predictors_waterbird_fall$PREDICTOR_NUM))
  r <- terra::rast(matrix(sample(codenums, size = 1000, replace = TRUE),
                          ncol = 100, nrow = 100))
  r2 = terra::subst(r, from = 15, to = NA) # remove all riparian
  test_wat = suppressWarnings(classify_landcover(r2, SDM = 'waterbird_fall', verbose = F))
  expect_warning(python_focal_prep(test_wat, SDM = 'waterbird_fall'))
})

# MASKING--------------
# test ability to mask another raster (e.g., surface water data) by the presence
# of each land cover class

codenums = DeltaMultipleBenefits::key$CODE_NUM
r <- terra::rast(matrix(sample(codenums, size = 10000, replace = TRUE),
                        ncol = 100, nrow = 100))
r2 = suppressWarnings(classify_landcover(r, SDM = 'waterbird_win', verbose = FALSE))
names(r2) = 'baseline'
w = r2
levels(w) = NULL
terra::coltab(w) = NULL
terra::values(w) <- sample(c(0,1), size = 10000, replace = TRUE) # simulate surface water data
names(w) = 'pwater'

test_that('two suffixes must be provided to avoid an error', {
  expect_error(
    expect_warning(python_focal_prep(r2, SDM = 'waterbird_fall', pixel_value = 0.09, mask = w)))
})

test_that('masking works as expected', {
  suffix = c('_area', '_pfld')
  pfld = suppressWarnings(python_focal_prep(r2, SDM = 'waterbird_fall', pixel_value = 0.09,
                      subset = w, suffix = suffix))
  expect_true(length(pfld) == length(suffix))
  expect_true(terra::nlyr(pfld$presence$baseline) == nrow(terra::freq(r2)))
  expect_true(terra::nlyr(pfld$presence_mask$baseline) == nrow(terra::freq(r2)))
})


