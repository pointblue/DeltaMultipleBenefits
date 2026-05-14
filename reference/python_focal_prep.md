# Prepare landscape rasters for focal statistics via Python

Prepare for running focal statistics on a landscape raster via Python,
to generate inputs for use with species distribution models.

## Usage

``` r
python_focal_prep(
  landscape,
  SDM,
  pathout,
  landscape_name,
  suffix = NULL,
  mask = NULL,
  pixel_value = NULL,
  overwrite = FALSE
)
```

## Arguments

- landscape:

  SpatRaster created by
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)

- SDM:

  The name of intended species distribution model, for which `landscape`
  will be reclassified: `"riparian"`, `"waterbird_fall"`,
  `"waterbird_win"`, or `"tima"`

- pathout, landscape_name:

  Character strings defining the filepath (`pathout/SDM/landscape_name`)
  where output rasters should be written

- suffix:

  Character string; custom suffix appended to layer names (optional
  unless `mask` is not `NULL`)

- mask:

  Optional SpatRaster; see Details

- pixel_value:

  Numeric value to replace cell values with (optional); default `NULL`

- overwrite:

  Logical; passed to
  [`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html);
  default `FALSE`

## Value

Nothing returned to R environment. Writes rasters to `pathout` for each
land cover class.

## Details

Splits landscape raster into separate layers representing the presence
(1) or absence (0) of each land cover class, then regroups and renames
them into the land cover classes used in by the intended species
distribution model (`SDM`), with an optional custom `suffix` appended to
the layer name. Cell values representing land cover presence (1) can
also optionally be replaced with a different `pixel_value` (e.g., the
area of each pixel).

By providing a `mask`, this function can also optionally use the land
cover presence layers as a mask to extract the values of another layer
(e.g., surface water data). To distinguish these layers, `suffix` is
required to have two values. See examples.

## See also

[`python_focal_run()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/python_focal_run.md),
[`python_focal_finalize()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/python_focal_finalize.md)

## Examples

``` r
#f <- system.file("ex/elev.tif", package="terra")
#r <- terra::rast(f) # add an example
#python_prep(landscape = r, SDM = 'riparian', pathout = 'example')

#try(python_prep(landscape = r, SDM = 'waterbird_win', pathout = 'example',
#pixel_value = 0.09, mask = system.file('ex/elev.tif', package = 'terra')))
## suffix is required if mask is not `NULL`

#python_prep(landscape = r, SDM = 'waterbird_win', pathout = 'example',
#pixel_value = 0.09, mask = system.file('ex/elev.tif', package = 'terra'),
#suffix = c('_area', '_elev'))
```
