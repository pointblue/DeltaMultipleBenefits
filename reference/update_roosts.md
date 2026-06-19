# Update waterbird predictors: crane roost locations

Helper function for estimating impact of landscape changes on the
locations of known crane roosts.

## Usage

``` r
update_roosts(
  landscape,
  unsuitable = c(11:19, 60, 70:79, 100:120),
  proportion = 0.2,
  roosts,
  dir,
  landscape_name,
  overwrite = FALSE
)
```

## Arguments

- landscape:

  SpatRaster created by
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)

- unsuitable:

  optional vector of numerical values representing the land cover
  classifications that should be considered incompatible with crane
  roosts; default values for the original land cover encoding include
  orchard & vineyard classes (11-19), urban (60), riparian classes
  (70-79), and woodland & scrub classes (100-120)

- proportion:

  numerical value for the proportion cover by an unsuitable land cover
  class at which the roost should be considered unsuitable; see Details

- roosts:

  SpatVector created by
  [`terra::vect()`](https://rspatial.github.io/terra/reference/vect.html)
  or character string giving the filepath to polygons representing the
  location of traditional crane roosts; expects attribute called
  "Roost_ID"

- dir, landscape_name:

  Character strings defining the filepath (`dir/landscape_name`) where
  updated roost location rasters should be written

- overwrite:

  Logical; passed to
  [`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html);
  default `FALSE`

## Details

For landscapes that represent a projected change from baseline
conditions, this function facilitates evaluating historical crane roosts
to determine whether the land cover overlaying them in `landscape` is
projected to become unsuitable, based on exceeding a threshold
`proportion` covered by an unsuitable land cover class. Unsuitable roost
polygons are removed, and updated roost maps are generated, named as
`scenario_name` in `pathout`. Use this function prior to using
[python_dist](https://pointblue.github.io/DeltaMultipleBenefits/reference/python_dist.md)
to calculate distance to roost and generate updated versions of
`droost_km.tif` for each scenario.

The default values for `unsuitable` include the original encodings for
orchard, vineyard, riparian, woodland, scrub, and urban land cover
classes, and the default threshold value for `proportion` is 0.2.
Alternate values can be provided as desired.

## See also

[`update_covertype()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/update_covertype.md),
[`update_pwater()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/update_pwater.md)

## Examples

``` r
# See vignette
```
