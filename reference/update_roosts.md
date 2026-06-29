# Update waterbird predictors: crane roost locations

Helper function for estimating impact of landscape changes on the
locations of known crane roosts.

## Usage

``` r
update_roosts(
  x,
  unsuitable = c(10:19, 60, 70:79, 100:120, 170:187),
  proportion = 0.2,
  roosts,
  filename = NULL,
  ...
)
```

## Arguments

- x:

  SpatRaster

- unsuitable:

  numerical vector representing the land cover classifications in `x`
  that are incompatible with crane roosts

- proportion:

  numerical threshold at which cover by an unsuitable land cover class
  makes a roost polygon unsuitable; see Details

- roosts:

  SpatVector or character string giving the filepath to polygons
  representing the location of traditional crane roosts

- filename:

  Optional character string passed to
  [`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html)

- ...:

  Additional arguments passed to
  [`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html)

## Details

For landscapes that represent a projected change from baseline
conditions, this function facilitates evaluating historical crane roost
polygons to determine whether the land cover overlaying them in raster
`x` is now unsuitable, based on exceeding a threshold `proportion`
covered by an unsuitable land cover class. Unsuitable roost polygons are
removed and the rest rasterized to match `x`. Use this function prior to
using
[`python_dist()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/python_dist.md)
to calculate distance to roost and generate updated versions of
`droost_km.tif` for each scenario.

The default values for `unsuitable` include the original encodings for
orchard and vineyard classes (10-19), urban (60), riparian (70-79,
170-187), and woodland and scrub (100-120), and the default threshold
value for `proportion` is 0.2.

## See also

[`update_covertype()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/update_covertype.md),
[`update_pwater()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/update_pwater.md)

## Examples

``` r
# See vignette
```
