# Create raster stack representing SDM predictors

Prepare for fitting SDMs by transforming a landscape raster into a stack
of rasters representing the required predictors.

## Usage

``` r
create_predictor_stack(x, SDM)
```

## Arguments

- x:

  SpatRaster

- SDM:

  The name of intended species distribution model: `"riparian"`,
  `"waterbird_fall"`, `"waterbird_win"`, or `"tima"`

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
SDM. See
[`classify_landcover.SpatRaster()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/classify_landcover.SpatRaster.md).

A warning is given if land cover classes expected by the model are
absent from the landscape, and rasters with all zero values are created
to represent them. However, the input raster should be carefully
reviewed to ensure they are truly absent and have not bee excluded from
the landscape unintentionally. If needed, the resulting layers can be
replaced manually before proceeding with
[`python_focal_run()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/python_focal_run.md).

## See also

[`classify_landcover()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/classify_landcover.md)

## Examples

``` r
r <- terra::rast(matrix(sample(c(11,19,71,72,90), size = 100, replace = TRUE),
         ncol = 10, nrow = 10))
r = suppressWarnings(create_predictor_stack(r, SDM = 'riparian'))
#> ORCHVIN AG RICE IDLE GRASSPAS URBAN POFR QULO SALIX MIXEDFOREST INTROSCRUB SALIXSHRUB MIXEDSHRUB PERM WATER BARREN WOODLAND&SCRUB
```
