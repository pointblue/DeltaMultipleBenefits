# Update waterbird predictors: pwater & pfld

Helper function for updating `pwater` and `pfld` predictors for the
waterbird distribution models. Generates file `pwater.tif` at locations
`pathout/pwater/landscape_name` and `pathout/SDM/landscape_name`.

## Usage

``` r
update_pwater(
  waterdat,
  mask = NULL,
  pathout,
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

- pathout, SDM, landscape_name:

  Character strings defining the filepath (`pathout/SDM/landscape_name`)
  where output rasters should be written; landscape_name should either
  correspond to the landscape represented by `waterdat` or the
  `scenario_landscape`, if given; see Details

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

## Value

Nothing; all files written to `pathout`

## Details

The waterbird distribution models incorporate information about surface
water data in two ways: as `pwater`, the expected probability of open
surface water in each cell of the landscape raster, specific to the
waterbird season being modeled and perhaps averaged over multiple years,
and as `pfld` focal statistics which represent the proportion of each
land cover class within a given distance of each cell that is flooded
(see
[`python_focal_prep()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/python_focal_prep.md)
and
[`python_focal_run()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/python_focal_run.md)).
Therefore, `pwater` data must be available for every landscape under
analysis before the `pfld` focal statistics can be generated and
distribution models fit.

Due to the dual needs for generating `pwater` and `pfld` predictors,
this function writes results in two places within `pathout`. The first
will be written to `pathout/pwater/landscape_name`, intended for later
use with
[`python_focal_prep()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/python_focal_prep.md)
and generating `pfld` predictors. The second will be written to
`pathout/SDM/landscape_name`, which is expected to be a directory
containing all final predictors for later use with
[`fit_SDM()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/fit_SDM.md)
in fitting waterbird models.

In addition, this function has two modes of operation. If
`scenario_landscape` is not provided, the `waterdat` is assumed to to
represent `pwater` data for the `landscape_name`, and is simply renamed
and copied to both `pathout` locations for use in later steps of
analysis, optionally masking before `pathout/SDM/landscape_name` is
written. The `mask` is never applied to the
`pathout/pwater/landscape_name` output intended for later focal
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
Tracker](https://www.pointblue.org/autowater) and may be downloaded from
[doi:10.5281/zenodo.7672193](https://doi.org/10.5281/zenodo.7672193).

## See also

[`update_covertype()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/update_covertype.md),
[`update_roosts()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/update_roosts.md)

## Examples

``` r
# See vignette
```
