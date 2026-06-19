# codenums = DeltaMultipleBenefits::key$CODE_NUM
#
# # TEST THAT IT WORKS:
# r <- terra::rast(matrix(sample(codenums, size = 1000, replace = TRUE),
#         ncol = 100, nrow = 100))
# test_rip = classify_landcover(r, SDM = 'riparian')
# #plot(test_rip)
#
# test_rip2 = create_predictor_stack(test_rip, SDM = 'riparian')
# plot(test_rip2)

# TEST ERRORS:
test_that('warnings given', {
  codenums = unique(na.omit(DeltaMultipleBenefits::predictors_waterbird_fall$PREDICTOR_NUM))
  r <- terra::rast(matrix(sample(codenums, size = 1000, replace = TRUE),
                        ncol = 100, nrow = 100))
  # remove all riparian
  r2 = terra::subst(r, from = 15, to = NA)
  test_wat = suppressWarnings(classify_landcover(r2, SDM = 'waterbird_fall', verbose = F))
  expect_warning(create_predictor_stack(test_wat, SDM = 'waterbird_fall'))
})

