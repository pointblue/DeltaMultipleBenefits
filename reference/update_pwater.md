# Update waterbird predictors: pwater & pfld

Helper function for updating `pwater` and `pfld` predictors for the
waterbird distribution models. Generates file `pwater.tif` at locations
`dir_focal/pwater/landscape_name` (for use with
[`python_focal_stats()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/python_focal_stats.md)
and `dir_final/SDM/landscape_name` (for use with
[`fit_SDM()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/fit_SDM.md).

## Usage

``` r
update_pwater(
  waterdat,
  mask = NULL,
  dir_focal = NULL,
  dir_final = NULL,
  SDM,
  landscape_name,
  overwrite = FALSE,
  baseline_landscape = NULL,
  scenario_landscape = NULL,
  floor = FALSE
)
```

## Arguments

- waterdat:

  `SpatRaster` or character string giving the filepath to a raster
  representing the probability of open water (pwater) in each cell,
  specific to the time frames appropriate to each `scenario_landscape`
  and waterbird SDM (i.e., fall vs. winter)

- mask:

  Optional `SpatRaster` or character string giving the filepath to a
  raster that should be used to mask the output, e.g. a study area
  boundary

- dir_focal:

  Optional string representing directory passed to
  [`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html),
  as (`dir_focal/pwater/landscape_name`). See Details.

- dir_final:

  Optional string representing directory passed to
  [`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html),
  as (`dir_final/SDM/landscape_name`). See Details.

- SDM:

  The name of intended species distribution model: either
  `"waterbird_fall"` or `"waterbird_win"`

- landscape_name:

  Character strings defining the filepath where output rasters will be
  written; should either correspond to the landscape represented by
  `waterdat` or the `scenario_landscape`, if given; see Details

- overwrite:

  Logical; passed to
  [`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html);
  default `FALSE`

- baseline_landscape, scenario_landscape:

  Optional SpatRasters created by
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  to compare with each other for estimating `pwater` for the changed
  portions of the `scenario_landscape`; see Details

- floor:

  Logical; if `TRUE`, don't allow new values of pwater to be lower than
  baseline values

- ...:

  Additional arguments passed to
  [`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html)

## Value

SpatRaster

## Details

The waterbird distribution models incorporate information about surface
water data in two ways: as `pwater`, the expected probability of open
surface water in each cell of the landscape raster, specific to the
waterbird season being modeled and perhaps averaged over multiple years,
and as `pfld` focal statistics which represent the proportion of each
land cover class within a given distance of each cell that is flooded.
Therefore, `pwater` data must be available for every landscape under
analysis before the `pfld` focal statistics can be generated and
distribution models fit.

Due to the dual needs for generating `pwater` and `pfld` predictors,
this function writes results in two places: The first will be written to
`dir_focal/pwater/landscape_name`, intended for later use with
[`python_focal_prep()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/python_focal_prep.md)
and generating `pfld` predictors. The second will be written to
`dir_final/SDM/landscape_name`, which is expected to be a directory
containing all final predictors for later use with
[`fit_SDM()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/fit_SDM.md)
in fitting waterbird models.

In addition, this function has two modes of operation. If
`scenario_landscape` is not provided, the `waterdat` is assumed to
represent `pwater` data for the `landscape_name`, and is simply renamed
and copied to both `dir_focal` and `dir_final` locations for use in
later steps of analysis, optionally masking before
`dir_final/SDM/landscape_name` is written. The `mask` is never applied
to the `pathout/pwater/landscape_name` output intended for later focal
statistics to avoid errors in processing near the boundaries of the
study area.

Alternatively, in the second mode, if both `baseline_landscape` and
`scenario_landscape` rasters are provided, this function will estimate
new `pwater` values for cells in the `scenario_landscape` that have
changed cover class, based on the mean probability of open surface water
for that land cover class in the `baseline_landscape`. Optionally, if
`floor = TRUE`, new probabilities of open water will be assigned only if
they are higher than the baseline values. In this mode, the result
represents `pwater` for the `scenario_landscape`, and `landscape_name`
should reflect the name of the scenario.

The original `pwater` baseline data used in the development of these
models was derived from Point Blue's [Water
Tracker](https://www.pointblue.org/autowater). See [Supporting
Information](https://pointblue.github.io/DeltaMultipleBenefits/reference/articles/supporting_information.md)
to download the original historical flooding data used in developing
these models

## See also

[`update_covertype()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/update_covertype.md),
[`update_roosts()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/update_roosts.md)

## Examples

``` r
# See vignette
```
