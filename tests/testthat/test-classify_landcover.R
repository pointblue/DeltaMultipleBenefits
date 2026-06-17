test_that('incomplete land covers produces a warning message', {
  r <- terra::rast(matrix(sample(c(11,19,71,72,90), size = 100, replace = TRUE),
                          ncol = 10, nrow = 10))
  expect_warning(classify_landcover(r, SDM = 'riparian', verbose = FALSE))

})

# include 50 = generic "GRASSLAND&PASTURE" which should be excluded from waterbird models
test_that('incomplete land covers & unused land covers produce two warning messages', {
  r <- terra::rast(matrix(sample(c(11,19,71,72,50), size = 100, replace = TRUE),
                          ncol = 10, nrow = 10))
  expect_warning(
    expect_warning(
      classify_landcover(r, SDM = 'waterbird_fall', verbose = FALSE)))
})

# library(sf)
# gis_proj = "C:/Users/kdybala/OneDrive - Point Blue/Documents/ArcGIS/Projects/DeltaMultipleBenefits_Phase2"
# veg_baseline_rast = terra::rast(file.path(gis_proj, 'landcover/VEG_baseline_Phase2.tif')) #version in gis_proj has levels defined; version in shared gdb does not!
#
# gis_ref = "C:/Users/kdybala/OneDrive - Point Blue/Documents/GIS_reference"
# lspt = read_sf(
#   file.path(gis_ref,
#             'LSPT_v3.0.0_SFEI/data/LSPT_data_package_SFEI/LSPT_data_package_SFEI.gdb'),
#   'Habitat_types_modern_forLSPT',
#   fid_column_name = 'FID')
# lspt_proj = st_transform(lspt, crs = crs(veg_baseline_rast))
#
# test = classify_landcover(lspt_proj)
# test_that('no land covers left behind', {
#   expect_false(any(is.na(test$CODE_NUM)))
#   })

# test |> st_drop_geometry() |> dplyr::filter(is.na(CODE_NUM)) |>
#   dplyr::select(Source_classification, Source, Habitat_Type, Crop2016) |>
#   dplyr::distinct()
# test |> st_drop_geometry() |> dplyr::arrange(CODE_NUM) |>
#   dplyr::pull(CODE_NUM) |> unique()

# test_rast = terra::rasterize(test, veg_baseline_rast, field = 'CODE_NUM')
# plot(test_rast)
#
# test2 = classify_landcover(test_rast, SDM = 'riparian', coltab = TRUE)
# plot(test2)
#
# test3 = classify_landcover(test_rast, SDM = 'waterbird_fall', coltab = TRUE)
# plot(test3)
#
# test4 = classify_landcover(test_rast, SDM = 'waterbird_win', coltab = TRUE)
# plot(test4)
#
# test5 = classify_landcover(test_rast, SDM = 'tima', coltab = TRUE)
# plot(test5)

# # test a deliberately broken raster
# test_rast_error = test_rast
# levels(test_rast_error) = NULL
# test_rast_error = subst(test_rast_error, from = c(170:189), to = 250) #remove all riparian and change to unknown value
# test6 = classify_landcover(test_rast_error, SDM = 'riparian')
#
# test_rast_error = test_rast
# levels(test_rast_error) = NULL
# test_rast_error = subst(test_rast_error, from = c(170:189), to = 10) #remove all riparian and change to PNAG
# test7 = classify_landcover(test_rast_error, SDM = 'riparian')
# plot(test7)


