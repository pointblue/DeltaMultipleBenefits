# Predictors used in distribution models for tidal marsh bird focal species

Predictor names and how they crosswalk to the major land cover classes
and subclasses listed in the vegetation `key`. Land cover classes or
subclasses with `NA` values in `PREDICTOR_NAME` and `PREDICTOR_NUM` are
excluded from these models. This data set is primarily for use in the
`reclassify_landcover()` function.

## Usage

``` r
predictors_tima
```

## Format

### `key` A data frame with 81 rows and 9 columns:

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

- WETL:

  Additional predictor grouping wetland subclasses together; 1 indicates
  the included subclasses

- NWET:

  Additional predictor grouping nontidal wetland subclasses together; 1
  indicates the included subclasses

- TWET:

  Additional predictor grouping tidal wetland subclasses together; 1
  indicates the include subclasses

- RFOR:

  Additional predictor grouping riparian forest subclasses together; 1
  indicates the included subclasses

- RSCR:

  Additional predictor grouping riparian scrub subclasses together; 1
  indicates the included subclasses

## Source

Dybala et al. In review
