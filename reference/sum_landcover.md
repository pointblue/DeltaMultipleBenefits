# Summarize total area of land cover classes

Summarize the area or number of pixels of each land cover class from a
set of landscape rasters

## Usage

``` r
sum_landcover(
  landscapes,
  mask = NULL,
  zones = NULL,
  pixel_area = 1,
  rollup = TRUE
)
```

## Arguments

- landscapes:

  Either a `SpatRaster` with one or more layers, or a character string
  giving the filepath to a directory containing one or more raster
  landscapes (e.g. scenarios) to be summarized.

- mask:

  Optional `SpatRaster` or character string giving the filepath to a
  raster that should be used to mask the output, e.g. a study area
  boundary

- zones:

  Optional `SpatRaster` or character string giving the filepath to a
  raster encoding zones within which pixel values should be summarized

- pixel_area:

  Numeric value representing the area of each pixel; hectares preferred
  but any units may be provided; default is 1, resulting in a total
  count

- rollup:

  Logical; whether or not riparian and managed wetland subclasses should
  be rolled up into a total area of riparian and managed wetlands,
  respectively. See Details.

## Value

tibble

## Details

This function will summarize the frequency of pixels of each land cover
class in each landscape raster, either provided as a `SpatRaster`
created by
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
with one or more layers, or as a filepath to a directory containing one
or more `tif` raster files to be summarized. This function is generally
agnostic as to the names of the rasters and the land cover classes it
expects, but it does expect the rasters to have unique names and to have
land cover class levels defined (i.e., not just contain numeric codes;
see
[`terra::levels()`](https://rspatial.github.io/terra/reference/factors.html)).
However, if `rollup = TRUE` (the default), riparian and wetland
subclasses are expected to have names that begin with `'RIPARIAN_'` or
`'WETLAND_'`. These subclasses will be summarized into a total area of
`'RIPARIAN'` and `'WETLAND'` land cover classes.

This function relies on
[`terra::freq()`](https://rspatial.github.io/terra/reference/freq.html)
to summarize the total number of cells of each value within each raster
and optionally each zone provided, which are then multiplied by
`pixel_area` as an estimate of the total area of each land cover class
in each landscape. The value for `pixel_area` can be provided in any
units you prefer, but if this function is intended to produce `areadat`
for use with
[`sum_metrics()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/sum_metrics.md),
we recommend using hectares to ensure alignment with the units in which
metrics were estimated.

By default, the total area of each unique land cover class will be
summarized for the entire raster provided, but optionally, the area can
be subset by providing a `mask` and/or `zones`. A `mask` should be a
single `SpatRaster` or filepath to a `tif` raster file that will be
passed to
[`terra::mask()`](https://rspatial.github.io/terra/reference/mask.html)
to exclude any areas with `NA` values. In addition, `zones` should be a
single `SpatRaster` or filepath to a `tif` raster file that will be
passed to
[`terra::zonal()`](https://rspatial.github.io/terra/reference/zonal.html).
Values in `zones` should define two or more unique regions within which
the area of each land cover class should be summarized, and the names of
each region should be defined (see
[`terra::levels()`](https://rspatial.github.io/terra/reference/factors.html)).

## See also

[`sum_habitat()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/sum_habitat.md),
[`sum_metrics()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/sum_metrics.md)

## Examples

``` r
# See vignette
```
