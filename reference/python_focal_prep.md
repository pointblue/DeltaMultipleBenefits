# Prepare landscape rasters for focal statistics via Python

Prepare for running focal statistics on a landscape raster via Python,
to generate inputs for use with species distribution models.

## Usage

``` r
python_focal_prep(
  x,
  SDM,
  pathout = NULL,
  landscape_name = NULL,
  suffix = NULL,
  mask = NULL,
  pixel_value = NULL,
  overwrite = FALSE
)
```

## Arguments

- x:

  SpatRaster

- SDM:

  The name of intended species distribution model, for which `x` will be
  reclassified: `"riparian"`, `"waterbird_fall"`, `"waterbird_win"`, or
  `"tima"`

- pathout, landscape_name:

  Optional character strings defining the filepath
  (`pathout/SDM/landscape_name`) where output rasters should be written

- suffix:

  Character string; custom suffix appended to layer names (optional
  unless `mask` is not `NULL`); see Details.

- mask:

  Optional SpatRaster; see Details

- pixel_value:

  Numeric value to replace cell values with (optional); default `NULL`

- overwrite:

  Logical; passed to
  [`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html);
  default `FALSE`

## Value

SpatRaster, though primarily used to write layers to file for use with
[`python_focal_run()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/python_focal_run.md)

## Details

This is a wrapper function that calls
[`create_predictor_stack()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/create_predictor_stack.md)
to split a land cover raster into separate layers representing the
presence (1) or absence (0) of each land cover class for use in
calculating focal statistics. See documentation of that function for
information about Warning messages.

Optionally, a custom `suffix` can be appended to the layer name and cell
values representing land cover presence (1) can be replaced with a
different `pixel_value` (e.g., the area of each pixel) as needed before
writing the layers to `pathout/SDM/landscape_name`.

By providing a `mask`, this function can also use the land cover
presence layers as a mask to extract the values of another layer (e.g.,
surface water data). To distinguish land cover presence from the values
extracted from another layer, `suffix` is required to have two values.
See examples.

## See also

[`python_focal_run()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/python_focal_run.md),
[`python_focal_finalize()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/python_focal_finalize.md)

## Examples
