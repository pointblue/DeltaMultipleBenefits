# Final processing of focal stats for SDMs

Renames and rescales output from
[`python_focal_run()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/python_focal_run.md)
as needed to match expected inputs for species distribution models
(SDMs). Includes options to mask by another raster and fill missing
values with zero.

## Usage

``` r
python_focal_finalize(
  pathin,
  landscape_name,
  SDM,
  scale,
  pathout,
  overwrite = FALSE,
  mask = NULL,
  cover = FALSE
)
```

## Arguments

- pathin, SDM, landscape_name, scale:

  Character strings defining the filepath
  (`pathin/SDM/landscape_name,scale`) containing input rasters to be
  processed, such as those created from running
  [`python_focal_run()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/python_focal_run.md)

- pathout:

  Character string defining the filepath (`pathout/SDM/landscape_name`)
  where output rasters should be written

- overwrite:

  Logical; passed to
  [`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html)

- mask:

  Optional `SpatRaster` or character string giving the filepath to a
  raster that should be used to mask the output, e.g. a study area
  boundary

- cover:

  Logical; default is `FALSE`. If `TRUE`, `mask` must not be NULL; See
  Details.

## Value

Nothing returned to R environment. Writes rasters to `pathout` for each
land cover class.

## Details

Function expects source files to be in a directory structure created by
[`python_focal_run()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/python_focal_run.md),
which is also used to inform the final processing steps:
`pathin/SDM/landscape_name/scale`. All .tif files in this source
directory will be read in, and optionally masked by the raster at
`mask`. If `cover = TRUE`, pixels in `mask` with a value of 1 will also
be replaced with a value of 0, and passed to
[`terra::cover()`](https://rspatial.github.io/terra/reference/cover.html)
to fill in missing values in source data with zero.

If `SDM = "riparian"`, pixel counts are converted to a proportion of the
total number of cells expected within the buffer distance represented by
`scale`, and the `scale` is appended to the predictor name in the format
"\_50" or "\_2000", as expected by the riparian bird SDMs.

If `SDM = "waterbird_fall"` or `SDM = "waterbird_win"`, the `scale` is
appended to the predictor name in the format "\_2k", "\_5k", or "\_10k",
as expected by the waterbird SDMs.

If `SDM = "tima"`, the scale is appended to the predictor name in the
format "100" or "2000", as expected by the tidal marsh bird SDMs.

The final rasters are then written to the directory
`pathout/SDM/landscape_name`, which will be created if it doesn't yet
exist.

## See also

[`python_focal_prep()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/python_focal_prep.md),
[`python_focal_run()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/python_focal_run.md)

## Examples

``` r
# See vignette
```
