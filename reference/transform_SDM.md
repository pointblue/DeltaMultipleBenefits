# Transform predictions from species distribution models to binary

Use model-specific threshold values to transform predicted probabilities
of species presence, such as resulting from
[`fit_SDM()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/fit_SDM.md),
to binary predictions of presence or absence.

## Usage

``` r
transform_SDM(
  pathin,
  SDM,
  landscape_name,
  regex = ".tif$",
  modlist,
  stat,
  pathout,
  overwrite = FALSE
)
```

## Arguments

- pathin, SDM, landscape_name:

  Character strings defining the filepath (`pathin/SDM/landscape_name`)
  containing the predicted probability of presence resulting from each
  distribution model, such as those created from running
  [`fit_SDM()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/fit_SDM.md)

- regex:

  Passed to `list.files` for selecting a subset of rasters from
  `pathin/SDM/landscape_name`; default is ".tif\$"

- modlist:

  List of model objects of class 'gbm' representing the distribution
  models to which new predictors should be fit.

- stat:

  Character string defining the threshold statistic to be used; see
  [`dismo::threshold()`](https://rdrr.io/pkg/dismo/man/threshold.html)
  for options

- pathout:

  Filepath for the directory where results rasters should be written
  (`pathout/SDM/landscape_name`)

- overwrite:

  Logical; passed to
  [`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html)

## Value

Nothing returned to R environment. Writes rasters to
`pathout/SDM/landscape_name` for each model in `modlist`

## See also

[`fit_SDM()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/fit_SDM.md)

## Examples

``` r
# See vignette
```
