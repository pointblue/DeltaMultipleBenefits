# Classify landcover for SpatRaster object

Prepare for fitting SDMs by reclassifying an existing landscape raster
according to the classifications used by a specific set of species
distribution models (SDM).

## Usage

``` r
# S3 method for class 'SpatRaster'
classify_landcover(x, SDM, coltab = TRUE, verbose = TRUE, ...)
```

## Arguments

- x:

  SpatRaster

- SDM:

  The name of intended species distribution model: `"riparian"`,
  `"waterbird_fall"`, `"waterbird_win"`, or `"tima"`

- coltab:

  logical; if TRUE add default color palette

- verbose:

  logical; if TRUE then print details associated with warning messages

- ...:

  Unused

## Value

SpatRaster with the same number of layers as the input `x`

## Details

Calls on internal datasets to crosswalk from land cover classes listed
in the
[key](https://pointblue.github.io/DeltaMultipleBenefits/reference/key.md)
to the predictors expected by the selected SDM group. The input raster
should already be encoded with the land cover classes listed in the
[key](https://pointblue.github.io/DeltaMultipleBenefits/reference/key.md).
To help with creating such a raster, see
[`classify_landcover.sf()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/classify_landcover.sf.md)
to map land cover polygons to the land cover classes in the
[key](https://pointblue.github.io/DeltaMultipleBenefits/reference/key.md).

A warning is given if there are land cover classes present in the
landscape that do not map to any of the predictors for the selected SDM
group, or if there are land cover classes missing from the landscape
that are expected by the selected SDM group. These warnings may
represent significant problems for fitting SDMs and should be carefully
reviewed. In either case, it is recommended to review the corresponding
internal datasets
([predictors_riparian](https://pointblue.github.io/DeltaMultipleBenefits/reference/predictors_riparian.md),
[predictors_waterbird_fall](https://pointblue.github.io/DeltaMultipleBenefits/reference/predictors_waterbird_fall.md),
[predictors_waterbird_win](https://pointblue.github.io/DeltaMultipleBenefits/reference/predictors_waterbird_win.md),
or
[predictors_tima](https://pointblue.github.io/DeltaMultipleBenefits/reference/predictors_tima.md))
for the list of expected predictors and how they map to land cover
classes in the
[key](https://pointblue.github.io/DeltaMultipleBenefits/reference/key.md).
Check whether the selected SDM group expects more specific land cover
classes or subclasses; the input raster may need to be reclassified
before proceeding.

## Examples

``` r
r <- terra::rast(matrix(sample(c(11,19,71,72,90), size = 100, replace = TRUE),
         ncol = 10, nrow = 10))
r <- suppressWarnings(classify_landcover(r, SDM = 'riparian'))
#> AG RICE IDLE GRASSPAS URBAN RIPARIAN SALIX MIXEDFOREST INTROSCRUB SALIXSHRUB MIXEDSHRUB WETLAND PERM BARREN WOODLAND&SCRUB
```
