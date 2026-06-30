# Apply species distribution models to new landscapes.

Fit previously-developed species distribution models for riparian
landbird species, waterbird groups during the fall or winter, or tidal
marsh bird species to a new set of predictors, such as those derived
from a new scenario of landscape change.

## Usage

``` r
fit_SDM(
  pathin,
  SDM,
  landscape_name,
  modlist,
  constants = NULL,
  factors = NULL,
  landscape = NULL,
  unsuitable = NULL,
  dir,
  overwrite = FALSE,
  ...
)
```

## Arguments

- pathin, SDM, landscape_name:

  Character strings defining the filepath (`pathin/SDM/landscape_name`)
  containing new predictor rasters to include in the model, such as
  those created from running
  [`python_focal_stats()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/python_focal_stats.md)

- modlist:

  List of model objects of class 'gbm' representing the distribution
  models to which new predictors should be fit.

- constants:

  optional dataframe containing predictors with a constant value that
  should be applied to all pixels. See Details.

- factors:

  optinal list of named lists defining categorical predictors included
  in distribution models. See Details.

- landscape:

  optional SpatRaster corresponding to the landscape represented by the
  predictors contained in `pathin/landscape_name`, used to identify the
  locations of `unsuitable` land covers. Must be provided if
  `unsuitable` is not `NULL`.

- unsuitable:

  optional vector of numerical values representing the land cover
  classifications that should be considered unsuitable *a priori*. If
  not `NULL`, `landscape` must also be provided.

- dir:

  Character string defining the filepath (`dir/SDM/landscape_name`)
  where output rasters should be written

- overwrite:

  Logical; passed to
  [`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html)

- ...:

  additional arguments passed to
  [`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html)

## Details

This function is designed to fit multiple distribution models to the
same set of predictors describing a given landscape. New predictors must
first be created and named to match the predictors included in the
original models, e.g. using
[`python_focal_prep()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/python_focal_prep.md)
and
[`python_focal_stats()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/python_focal_stats.md).

`constants` are passed to
[`terra::predict()`](https://rspatial.github.io/terra/reference/predict.html)
and provide a way to include constant values for one or more predictors
that should be applied to all pixels. For both riparian and waterbird
models, this will include a predictor representing effort ('area.ha' for
riparian landbirds and 'offset' for waterbirds). For riparian landbird
models applied only to the Delta, constants should also include a region
predictor used as a categorical predictor representing the Sacramento
Valley (0) or the Delta or San Joaquin Valley (1). None needed for tidal
marsh birds. (See vignette)

`factors` are also passed to
[`terra::predict()`](https://rspatial.github.io/terra/reference/predict.html)
and provide a way to define categorical predictors. For waterbird and
tidal marsh bird models, this is necessary to define the 'covertype' and
'LANDCOVER' predictors, respectively. (See vignette)

`unsuitable` land covers will be presumed to have a predicted value of
zero. The locations of `unsuitable` landcovers will be extracted from
`landscape`, assigned a value of zero, and overlaid on the model
predictions.

## See also

[`python_focal_prep()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/python_focal_prep.md),
[`python_focal_stats()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/python_focal_stats.md),
[`update_covertype()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/update_covertype.md),
[`update_pwater()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/update_pwater.md),
[`update_roosts()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/update_roosts.md),
[`python_dist()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/python_dist.md)

## Examples

``` r
# See vignette
```
