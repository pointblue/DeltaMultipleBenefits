
# example rasters for testing
# simulated land cover data:
r <- terra::rast(
  matrix(
    sample(c(11,19,22,26,30,52,56,60,90,100,172,184,196,213),
           size = 10000, replace = TRUE),
    ncol = 100, nrow = 100))
names(r) = 'baseline'

# simulated limited land cover data (no rice)
r_lim <- terra::rast(
  matrix(
    sample(c(11,19,22,26,52,56,60,90,100,172,184,196,213),
           size = 10000, replace = TRUE),
    ncol = 100, nrow = 100))
names(r_lim) = 'baseline_lim'

# simulated extraneous land cover data (include a number not in the key)
r_extr1 <- terra::rast(
  matrix(
    sample(c(11,19,52,90,100,221),
           size = 10000, replace = TRUE),
    ncol = 100, nrow = 100))
names(r_extr1) = 'baseline_extr1'



# simulated surface water data:
w <- terra::rast(matrix(sample(c(0,1), size = 10000, replace = TRUE),
                        ncol = 100, nrow = 100))
names(w) = 'pwater'

# classify_landcover.SpatRaster()--------
# include 50 = generic "GRASSLAND&PASTURE" which should be excluded from waterbird models
test_that('classify_landcover produces a warning for unused land covers: watfall', {
  expect_warning(
    classify_landcover(r_extr1, SDM = 'waterbird_fall', verbose = FALSE))
})

test_that('classify_landcover produces a warning for unused land covers: tima', {
  expect_warning(
    classify_landcover(r_extr1, SDM = 'tima', verbose = FALSE))
})

test_that('classify_landcover produces a warning for unused land covers: rip', {
  expect_warning(
    classify_landcover(r_extr1, SDM = 'riparian', verbose = FALSE))
})

# create_predictor_stack()-----------
test_that('create_predictor_stack produces a warning for missing land cover predictors: watfall', {
  expect_warning(
    create_predictor_stack(x = r_lim, SDM = 'waterbird_fall', fill = FALSE,
                           verbose = FALSE))
})

test_that('create_predictor_stack produces a warning for missing land cover predictors: tima', {
  expect_warning(
    create_predictor_stack(x = r_lim, SDM = 'tima', fill = FALSE,
                           verbose = FALSE))
})

test_that('create_predictor_stack produces a warning for missing land cover predictors: rip', {
  expect_warning(
    create_predictor_stack(x = r_lim, SDM = 'riparian', fill = FALSE,
                           verbose = FALSE))
})

# python_focal_prep()------------

# errors passed from create_predictor stack and classify_landcover.SpatRastser persist
test_that('warnings passed through', {
  expect_warning(
    python_focal_prep(r_lim, SDM = 'waterbird_fall', fill = FALSE, verbose = FALSE))
})

## ability to return a per-pixel value-------
test_that('pixel values are correctly applied', {
  area = suppressWarnings(
    python_focal_prep(r, SDM = 'waterbird_fall', pixel_value = 0.09,
                      fill = FALSE, verbose = FALSE))
  expect_true(all(unique(terra::values(area[[1]]) %in% c(0.00, 0.09, NaN))))
})

## ability to mask another raster by the presence of each land cover class----
test_that('two suffixes must be provided to avoid an error', {
  expect_error(
    expect_warning(
      expect_warning(
        python_focal_prep(r, SDM = 'waterbird_fall', pixel_value = 0.09,
                          subset = w, fill = FALSE, verbose = FALSE)
      )))

})

test_that('masking works as expected', {
  suffix = c('_area', '_pfld')
  pfld = suppressWarnings(
    python_focal_prep(r, SDM = 'waterbird_fall', pixel_value = 0.09,
                      subset = w, fill = FALSE, verbose = FALSE,
                      suffix = suffix))
  expect_true(length(pfld) == length(suffix))
})

# presence = purrr::map(
#   c(1:terra::nlyr(r)),
#   ~create_predictor_stack(x = r[[.x]], SDM = 'waterbird_fall', fill = FALSE, verbose = FALSE))
# landscape_names = names(r)
# names(presence) = landscape_names
# presence_mask = mask_predictors(lc = presence, masklayer = w, suffix = c('_area', '_pfld'))
# names(presence_mask) = landscape_names
