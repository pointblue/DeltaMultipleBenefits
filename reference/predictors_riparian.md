# Predictors used in distribution models for riparian focal species

Predictor names and how they crosswalk to the major land cover classes
and subclasses listed in the vegetation `key`. Land cover classes or
subclasses with `NA` values in `PREDICTOR_NAME` but `999` in
`PREDICTOR_NUM` indicate that they are included as part of a larger
grouping predictor, as flagged in the `RIPARIAN` or `WETLAND` columns.

## Usage

``` r
predictors_riparian
```

## Format

### `key` A data frame with 81 rows and 6 columns:

- CODE_NUM:

  Numeric value used to encode rasters, matching `key`

- CODE_NAME:

  Text string joining major land cover classes to subclasses with a
  '\_', matching `key`

- PREDICTOR_NAME:

  Corresponding predictor name in tidal marsh bird distribution models

- PREDICTOR_NUM:

  Corresponding numeric value used to reclassify land cover rasters for
  use with tidal marsh bird distribution models

- RIPARIAN:

  Additional predictor grouping riparian subclasses together; 1
  indicates the included subclasses

- WETLAND:

  Additional predictor grouping wetland subclasses together; 1 indicates
  the included subclasses

## Source

Dybala et al. 2023 (https://doi.org/10.15447/sfews.2023v21iss3art4)

## Details

This data set is primarily for use in the
[`reclassify_landcover()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/reclassify_landcover.md)
function.
