# Prepare landscape rasters for focal statistics via Python

Prepare for running focal statistics on a landscape raster via Python,
to generate inputs for use with species distribution models.

## Usage

``` r
python_focal_prep(
  landscape,
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

- landscape:

  SpatRaster created by
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)

- SDM:

  The name of intended species distribution model, for which `landscape`
  will be reclassified: `"riparian"`, `"waterbird_fall"`,
  `"waterbird_win"`, or `"tima"`

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
[`reclassify_landcover()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/reclassify_landcover.md)
to group and rename land covers in the provided landscape raster to
match predictors in the intended species distribution model and split
them into separate layers representing the presence (1) or absence (0)
of each land cover class for use in calculating focal statistics. See
documentation of that function for information about Warning messages.

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

``` r
library(DeltaMultipleBenefits)
r <- terra::rast(matrix(sample(c(11,19,71,72,90), size = 100, replace = TRUE),
         ncol = 10, nrow = 10))

#return the presence of each land cover class
presence = suppressWarnings(python_focal_prep(landscape = r, SDM = 'riparian'))
#> Step 1: Success (All land cover classes present in the provided landscape will be incorporated into the selected SDM)
#> AG RICE IDLE GRASSPAS URBAN SALIX MIXEDFOREST INTROSCRUB SALIXSHRUB MIXEDSHRUB PERM BARREN WOODLAND&SCRUB NOTES

#return the area of the pixel where each land cover class is present
#(useful for summing over moving windows)
area = suppressWarnings(python_focal_prep(landscape = r, SDM = 'waterbird_win', pixel_value = 0.09))
#> Step 1: Success (All land cover classes present in the provided landscape will be incorporated into the selected SDM)
#> ww grain corn field row rice fal alf ip dryp dev wet duwet barren for

#mask another raster (e.g., surface water data) by where each land cover is present
w = r
terra::values(w) <- sample(c(0,1), size = 100, replace = TRUE)
#pfld = python_focal_prep(landscape = r, SDM = 'waterbird_fall', pixel_value = 0.09, mask = w)
#returns error: two values for suffix are required if mask is not `NULL`
pfld = suppressWarnings(
   python_focal_prep(
      landscape = r, SDM = 'waterbird_fall', pixel_value = 0.09,
       mask = w, suffix = c('_area', '_pfld')))
#> Step 1: Success (All land cover classes present in the provided landscape will be incorporated into the selected SDM)
#> grain corn field row rice fal alf ip dryp dev wet duwet barren for
```
