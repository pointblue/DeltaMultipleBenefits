# Prepare landscape rasters for focal statistics via Python

Prepare for running focal statistics on a landscape raster via Python,
to generate inputs for use with species distribution models.

## Usage

``` r
python_focal_prep(
  x,
  SDM,
  fill = TRUE,
  suffix = NULL,
  pixel_value = NULL,
  subset = NULL,
  dir = NULL,
  overwrite = FALSE,
  ...
)
```

## Arguments

- x:

  SpatRaster with layer names corresponding to `landscape_name`

- SDM:

  The name of intended species distribution model, for which `x` will be
  reclassified: `"riparian"`, `"waterbird_fall"`, `"waterbird_win"`, or
  `"tima"`

- fill:

  logical; see Details

- suffix:

  Character string; custom suffix appended to layer names (optional
  unless `mask` is not `NULL`); see Details.

- pixel_value:

  Optional numeric value to use in place of 1 where land covers are
  present; default `NULL`

- subset:

  Optional SpatRaster or string representing filepath to a raster. See
  Details.

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

SpatRaster, though primarily used to write layers to file for use with
[`python_focal_stats()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/python_focal_stats.md)

## Details

This is a wrapper function that calls
[`create_predictor_stack()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/create_predictor_stack.md)
to split a land cover raster into separate layers representing the
presence (1) or absence (0) of each land cover class for use in
calculating focal statistics. See documentation of that function for
information about Warning messages.

If `suffix` is provided, it is appended to the layer name. If
`pixel_value` is provided, values representing land cover presence (1)
are replaced with this value (e.g., the area of each pixel).

If `subset` is provided, either as a SpatRaster or a filepath to a
raster, the `subset` layer is masked by the presence of each land cover
layer, e.g. to extract from surface water data the extent of surface
water within each land cover class. In this case, both the land cover
presence data and the subset data are returned; to distinguish between
these results, `suffix` is required to have two values. The first is
appended to the land cover presence data and the second is appended to
the subset data. See examples.

All output can be optionally written to `dir/SDM/landscape_name` where
`landscape_name` is taken from the names of the input raster(s) `x`.

## See also

[`python_focal_stats()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/python_focal_stats.md)

## Examples

``` r
codenums = DeltaMultipleBenefits::key$CODE_NUM
r <- terra::rast(matrix(sample(codenums, size = 1000, replace = TRUE), ncol = 100, nrow = 100))
watwin = suppressWarnings(classify_landcover(r, SDM = 'waterbird_win', verbose = FALSE))
watwin_pred = python_focal_prep(watwin, SDM = 'waterbird_win')

# return the area of the pixel where each land cover class is present
# (useful for summing over moving windows)
watwin_area = python_focal_prep(watwin, SDM = 'waterbird_win', pixel_value = 0.09)

# mask another raster (e.g., surface water data) by the presence of each
# land cover class:
w = watwin # simulate surface water data
levels(w) = NULL
terra::coltab(w) = NULL
terra::values(w) <- sample(c(0,1), size = 10000, replace = TRUE)
#pfld = python_focal_prep(watwin, SDM = 'waterbird_win', pixel_value = 0.09, mask = w)
#returns error because two suffixes need to be provided
pfld = python_focal_prep(watwin, SDM = 'waterbird_win', pixel_value = 0.09, mask = w,
                         suffix = c('_area', '_pfld')) # works
```
