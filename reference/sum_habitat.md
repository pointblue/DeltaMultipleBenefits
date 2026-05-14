# Summarize total habitat scores

Calculate the sum of the predicted presence/absence or probability of
presence from species distribution models corresponding to a set of
landscape rasters.

## Usage

``` r
sum_habitat(
  pathin,
  SDM = NULL,
  landscape_name = NULL,
  regex = ".tif$",
  zones = NULL,
  subtype = NULL,
  rollup = TRUE,
  key = NULL,
  scale = NULL
)
```

## Arguments

- pathin:

  Character string defining the filepath to the highest-level directory
  containing the predicted presence/absence or probability of presence
  from species distribution models, such as those created from running
  [`fit_SDM()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/fit_SDM.md)
  or
  [`transform_SDM()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/transform_SDM.md)

- SDM, landscape_name:

  Optional character strings defining the subdirectories within `pathin`
  for which habitat should be summarized; see Details.

- regex:

  Passed to `list.files` for selecting a subset of rasters from
  `pathin/SDM/landscape_name`; default is ".tif\$"

- zones:

  Optional `SpatRaster` or character string giving the filepath to a
  raster encoding zones within which pixel values should be summarized

- subtype:

  Optional character string appended to the field METRIC_SUBTYPE, such
  as for distinguishing probability of presence from presence/absence

- rollup:

  Logical; If `TRUE` (default), summarize total habitat across all
  species/groups by set of SDMs and add to output

- key:

  Optional tibble, dataframe, or character string defining the filepath
  passed to
  [`readr::read_csv()`](https://readr.tidyverse.org/reference/read_delim.html),
  used to translate the individual species names as encoded in the file
  names in pathin to readable METRIC names in the output table; see
  Details

- scale:

  Optional value by which to scale the results; see Details

## Value

tibble

## Details

By default, this function will calculate the sum of all pixel values in
each raster found in `pathin` and any subdirectories, to efficiently
process multiple rasters located in the `pathin` directory at once. The
file structure within `pathin` is used to infer the name of the
corresponding `SDM` and `landscape_name`. However, `SDM` or both `SDM`
and `landscape_name` can be optionally specified to only process a
subset of these. The `regex` option can also be used to specify a subset
of the rasters within `pathin/SDM/landscape_name`.

If SDM is one of the recognized models (`"riparian"`,
`"waterbird_fall"`, `"waterbird_win"`, or `"tima"`), the output will
produce nicer labels for the `METRIC` and `METRIC_SUBTYPE` fields.
Otherwise, `METRIC_SUBTYPE` will be filled with the `SDM` value.

If provided, `key` should refer to a tibble, dataframe, or filepath to a
CSV containing the fields `spp` and `label`, used for converting the
filenames of the rasters within `pathin` to a more readable label, which
will be renamed `METRIC` in the final output.

If provided, the sum of all pixel values will be multiplied by `scale`.
For example, to rescale the total in terms of the total area, enter the
area of each pixel.

## See also

[`fit_SDM()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/fit_SDM.md),
[`transform_SDM()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/transform_SDM.md)

## Examples

``` r
# See vignette
```
