# Calculate Euclidean distance via Python

Function to call python script for calculating Euclidean distances on
landscape rasters via arcpy.

## Usage

``` r
python_dist(
  pathin,
  landscape_name,
  pathout,
  SDM,
  filename = "droost_km.tif",
  scale = NULL,
  mask = NULL,
  overwrite = FALSE
)
```

## Arguments

- pathin, landscape_name:

  Character strings defining the filepath (`pathin/landscape_name`)
  where input rasters are located, such as those created from running
  [`python_focal_prep()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/python_focal_prep.md)
  or
  [`update_roosts()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/update_roosts.md)

- pathout, SDM:

  Additional character strings defining the filepath
  (`pathout/SDM/landscape_name`) where output raster should be written

- filename:

  name of the output raster, including file extension; default is
  'droost_km.tif', the name of the predictor required by the waterbird
  models

- scale:

  Optional character string for scaling the results; See Details

- mask:

  Optional `SpatRaster` or character string giving the filepath to a
  raster that should be used to mask the output, e.g. a study area
  boundary

- overwrite:

  Logical; passed to
  [`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html);
  does not apply to the intermediate step of writing `droost_raw.tif`

## Value

Nothing; all files written to `pathout/SDM/landscape_name`

## Details

Calls the `dist_stats.py` function to calculate the Euclidean distance
for all cells in the input raster without a value to the nearest cell
with a value (e.g., for calculating distance to a crane roost or a
stream).

Raw python results will be written to
`pathin/landscape_name/droost_raw.tif`, and then optionally scaled
and/or masked, before writing the final output to
`pathout/SDM/landscape_name/`. Currently supported scale options
include: `km` to divide the results by 1000 and return distances in
kilometers or `sqrt` to take the square root of the results.

Important: This function requires the availability of arcpy and Spatial
Analyst extensions. While these statistics can be entirely calculated in
R, arcpy is much faster. Note: the initial raw output from
`dist_stats.py` to `pathin/landscape_name/droost_raw.tif` will not
overwrite existing rasters; old versions must be deleted before
re-running.

## Examples

``` r
# See vignette
```
