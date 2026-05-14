# Update waterbird and tidal marsh bird predictors: covertype and LANDCOVER

Helper function for updating covertype and LANDCOVER predictors for the
waterbird and tidal marsh bird distribution models, respectively.

## Usage

``` r
update_covertype(
  landscape,
  key,
  SDM,
  mask = NULL,
  pathout,
  landscape_name,
  overwrite = FALSE
)
```

## Arguments

- landscape:

  SpatRaster created by
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)

- key:

  tibble, dataframe, or character string defining a filepath passed to
  [`readr::read_csv()`](https://readr.tidyverse.org/reference/read_delim.html),
  used to interpret the raster values in `landscape` to land cover class
  names; see Details

- SDM:

  The name of intended species distribution model: `"waterbird_fall"`,
  `"waterbird_win"`, or `"tima"`

- mask:

  Optional filepath to a raster that should be used to mask the output,
  e.g. a study area boundary

- pathout, landscape_name:

  Character strings defining the filepath (`pathout/SDM/landscape_name`)
  where output rasters should be written

- overwrite:

  Logical; passed to
  [`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html);
  default `FALSE`

## Value

Nothing; all files written to `pathout`

## Details

Classifies the `landscape` rasters according to the land cover classes
that were originally surveyed, which are the only classes for which
predictions should be generated from the waterbird distribution models.
For `"waterbird_fall"` or `"waterbird_win"` SDMs, generates file
`covertype.tif` and for `"tima"` SDMs, generates file `LANDCOVER.tif`.
Output is written to `pathout/SDM/scenario_name/`.

## See also

[`update_pwater()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/update_pwater.md);
[`update_roosts()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/update_roosts.md)

## Examples

``` r
# See vignette
```
