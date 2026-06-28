# Create raster stack representing SDM predictors

Prepare for fitting SDMs by transforming a landscape raster into a stack
of rasters representing the required predictors.

## Usage

``` r
create_predictor_stack(x, SDM, fill = TRUE)
```

## Arguments

- x:

  SpatRaster; can only have 1 layer

- SDM:

  The name of intended species distribution model: `"riparian"`,
  `"waterbird_fall"`, `"waterbird_win"`, or `"tima"`

- fill:

  logical; see Details

## Value

SpatRaster with separate layers for each land cover class included as a
predictor in the selected SDM representing the presence (1) and absence
(0)

## Details

This function is called by
[`python_focal_prep()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/python_focal_prep.md)
and is not intended to be called directly. Segregates a landscape raster
into separate layers representing each land cover class. Also calls on
internal datsets to create broader grouping variables as required for
the selected SDM. The input raster should already be classified
according to the land cover classifications expected by the selected
SDM.

A warning is given if there are land cover classes present in the
landscape that do not map to any of the predictors for the selected SDM
group (see
[`classify_landcover.SpatRaster()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/classify_landcover.SpatRaster.md)),
and separate warning is given if land cover classes expected by the
model are absent from the landscape. In the latter case, if
`fill = TRUE` (the default), the function will create additional layers
with all zero values for each missing land cover class. However, the
input landscape should be carefully reviewed to ensure they are truly
absent and have not been excluded from the landscape unintentionally. If
needed, the resulting layers can be replaced manually before proceeding
with
[`python_focal_run()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/python_focal_run.md).

## See also

[`classify_landcover()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/classify_landcover.md)

## Examples

``` r
r <- terra::rast(matrix(sample(c(11,19,71,72,90), size = 100, replace = TRUE),
         ncol = 10, nrow = 10))
r = suppressWarnings(create_predictor_stack(r, SDM = 'riparian'))
#> AG RICE IDLE GRASSPAS URBAN SALIX MIXEDFOREST INTROSCRUB SALIXSHRUB MIXEDSHRUB PERM BARREN WOODLAND&SCRUB
#> Because fill = TRUE, creating missing rasters with all zero values, butconfirm they are truly absent from the landscape.
```
