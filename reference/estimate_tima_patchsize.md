# Estimate tidal marsh patch size

Additional preparation required prior to running focal statistics on a
landscape raster via Python, to generate tidal wetland patch size
estimates for use with tidal marsh bird ('tima') models.

## Usage

``` r
estimate_tima_patchsize(
  x,
  directions = 8,
  zeroAsNA = TRUE,
  fill = FALSE,
  dir = NULL,
  overwrite = FALSE,
  ...
)
```

## Arguments

- x:

  SpatRaster or list of SpatRasters; see Details

- directions:

  integer passed to
  [`terra::patches()`](https://rspatial.github.io/terra/reference/patches.html)
  indicating which cells are considered adjacent. Should be 8 (Queen's
  case) or 4 (Rook's case)

- zeroAsNA:

  logical passed to
  [`terra::patches()`](https://rspatial.github.io/terra/reference/patches.html).
  If TRUE treat cells that are zero as if they were NA

- fill:

  logical. If TRUE replaces all non-tidal wetland vegetation with 0.

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

SpatRaster

## Details

The input should be a SpatRaster or list of SpatRasters resulting from
running
[`python_focal_prep()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/python_focal_prep.md)
with `SDM = "tima"`. This function extracts the `TWET` predictor layer,
representing all tidal wetland vegetation, identifies distinct
contiguous patches, and assigns each pixel within each patch a value
corresponding to the count of pixels within the patch.

## Examples

``` r
codenums = DeltaMultipleBenefits::key$CODE_NUM
r <- terra::rast(matrix(sample(codenums, size = 1000, replace = TRUE), ncol = 100, nrow = 100))
tima_pred = python_focal_prep(r, SDM = 'tima')
#> Warning: Caution Advised. Some land cover classes are not represented by any of the predictors for the selected SDM. Check input raster for errors.
#>                   CODE_NAME count  prop
#> 1                  RIPARIAN    80 0.008
#> 2           WETLAND_MANAGED   100 0.010
#> 3 WETLAND_MANAGED_PERENNIAL   110 0.011
#> 4  WETLAND_MANAGED_SEASONAL   130 0.013
#> 5             WETLAND_OTHER   150 0.015
#> PNAG AGGRPAS RICE URBN POFR QUER SALF MIXF INTR SALS MIXS WETLAND VERP WATER WOODY BARREN RIPARIAN TULE PHRA SALTPICK EMER MEAD LEPI ALKA
#> Warning: Extreme Caution Advised. Land cover classes are missing from the input raster but are expected by the selected SDM. Check input raster for errors.
#> Because fill = TRUE, creating missing rasters with all zero values, butconfirm they are truly absent from the landscape.
tima_psize = estimate_tima_patchsize(tima_pred)
```
