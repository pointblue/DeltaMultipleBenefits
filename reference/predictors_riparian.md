# Predictors used in distribution models for riparian focal species

Predictor names and how they crosswalk to the major land cover classes
and subclasses listed in the vegetation `key`. Land cover classes with a
`1` in the `RIPARIAN` or `WETLAND` fields indicate they should be
included in additional predictors representing the proportion cover of
those broader vegetation classes. This data set is primarily for use in
the
[`reclassify_landcover()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/reclassify_landcover.md)
function.

## Usage

``` r
predictors_riparian
```

## Format

### `predictors_riparian` A data frame with 81 rows and 7 columns:

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

- NOTES:

  See Details

## Source

Dybala et al. 2023 (https://doi.org/10.15447/sfews.2023v21iss3art4)

## Details

The `NOTES` field provides additional information on vegetation classes
that were ultimately excluded from model predictions and can be safely
ignored (i.e. the `WOODLAND&SCRUB` class), as well as notes on more
specific vegetation types from the original source classifications that
were excluded from the predictor when these models were first developed.
Therefore, use of this key to crosswalk land cover data to the
predictors for these models cannot be fully consistent with the original
classification scheme used to develop these models. See
`classify_LSPT()` for an alternative approach to classify the original
land cover polygons.
