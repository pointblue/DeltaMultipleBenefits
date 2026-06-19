# Update waterbird and tidal marsh bird predictors: covertype and LANDCOVER

Helper function for updating `covertype` and `LANDCOVER` predictors for
the waterbird ("waterbird_fall", "waterbird_win") and tidal marsh
("tima") distribution models, respectively.

## Usage

``` r
update_covertype(x, SDM, mask = NULL, dir = NULL, overwrite = FALSE, ...)
```

## Arguments

- x:

  SpatRaster

- SDM:

  The name of intended species distribution model: `"waterbird_fall"`,
  `"waterbird_win"`, or `"tima"`

- mask:

  Optional SpatRaster or string representing filepath to a raster that
  should be used to mask the output, e.g. a study area boundary

- dir:

  Optional string representing directory passed to
  [`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html),
  as (`dir/SDM/landscape_name`). See Details.

- overwrite:

  logical. If `TRUE`, output is overwritten

- ...:

  additional arguments passed to
  [`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html)

## Value

SpatRaster with the same number of layers as the input `x`, names
required by the selected `SDM`.

## Details

Classifies land cover rasters to generate categorical predictors
required by the "waterbird_fall", "waterbird_win", and "tima" models.
The input raster `x` should already represent land cover predictors
required by the selected `SDM`, i.e. output from
[`classify_landcover.SpatRaster()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/classify_landcover.SpatRaster.md).
A `mask` raster can optionally be provided, either as a SpatRaster or a
filepath to a raster, to first mask the input raster(s) `x`. Output can
be optionally written to `dir/SDM/landscape_name` where `landscape_name`
is taken from the names of the input raster(s) `x`.

## See also

[`classify_landcover.SpatRaster()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/classify_landcover.SpatRaster.md);
[`update_pwater()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/update_pwater.md);
[`update_roosts()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/update_roosts.md)

## Examples

``` r
# See vignette
```
