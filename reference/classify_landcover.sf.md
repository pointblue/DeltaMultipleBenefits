# Classify landcover data for simple features object

Prepare a land cover data set for use with the other functions in this
package by aligning the land cover classifications with those required
by the
[metrics](https://pointblue.github.io/DeltaMultipleBenefits/reference/metrics.md)
data as provided in the
[key](https://pointblue.github.io/DeltaMultipleBenefits/reference/key.md)
and corresponding predictors required for fitting species distribution
models.

## Usage

``` r
# S3 method for class 'sf'
classify_landcover(x, source = "LSPT", ...)
```

## Arguments

- x:

  object of class sf, sfc or sfg

- source:

  character string indicating the source of the land cover polygons;
  currently only "LSPT" is supported

- ...:

  Unused

## Details

This function provides support for cross-walking polygon-based
vegetation data, such as the "Habitat_types_modern_forLSPT" layer
published in the Landscape Scenario Planning Tool v3 (LSPT), to the land
cover classes required for use with this package. Ultimately, this
function may be updated to extend support to other land cover data
sources, but it currently relies on the fields provided in that layer,
especially: `Habitat_Type`, `Source_classification`, `Crop2016`, and,
`Tidal`.

The function returns an sf object with additional fields:

- `CODE_NAME`: most specific land cover class designation, matching
  those provided in the
  [key](https://pointblue.github.io/DeltaMultipleBenefits/reference/key.md)

- `CODE_NUM`: corresponding code number, matching those in the
  [key](https://pointblue.github.io/DeltaMultipleBenefits/reference/key.md),
  for creating rasters

- `PREDICTOR_RIPARIAN`: corresponding predictor name used in the
  riparian landbird SDMs

- `PREDICTOR_WATERBIRD_FALL`: corresponding predictor name used in the
  waterbird SDMs for the fall season

- `PREDICTOR_WATERBIRD_WIN`: corresponding predictor name used in the
  waterbird SDMs for the winter season

- `PREDICTOR_TIMA`: corresponding predictor name used in the tidal marsh
  bird SDMs

The `CODE_NAME` field can be rasterized for further analysis with the
[metrics](https://pointblue.github.io/DeltaMultipleBenefits/reference/metrics.md)
data and converted to predictors for fitting the SDMs by calling this
function again on the raster (see
[`classify_landcover.SpatRaster()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/classify_landcover.SpatRaster.md).
Alternatively, each predictor field can be rasterized directly from this
output. The results should be mostly identical, except for a few special
cases for the riparian predictors; see
[predictors_riparian](https://pointblue.github.io/DeltaMultipleBenefits/reference/predictors_riparian.md)
for more information.

## See also

[key](https://pointblue.github.io/DeltaMultipleBenefits/reference/key.md),
[predictors_riparian](https://pointblue.github.io/DeltaMultipleBenefits/reference/predictors_riparian.md),
[predictors_waterbird_win](https://pointblue.github.io/DeltaMultipleBenefits/reference/predictors_waterbird_win.md),
[predictors_waterbird_fall](https://pointblue.github.io/DeltaMultipleBenefits/reference/predictors_waterbird_fall.md),
[predictors_tima](https://pointblue.github.io/DeltaMultipleBenefits/reference/predictors_tima.md)
