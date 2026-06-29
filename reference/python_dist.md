# Calculate Euclidean distance via Python

Wrapper function to call a python script for calculating Euclidean
distances on landscape rasters. On first call this function imports the
arcpy module and Spatial Analyst extensions then sources the Python
script "dist_stats.py".

## Usage

``` r
python_dist(
  x,
  scale = NULL,
  mask = NULL,
  python = NULL,
  dir = NULL,
  SDM = NULL,
  landscape_name = NULL,
  filename = "droost_km.tif",
  ...
)
```

## Arguments

- x:

  Filepath or SpatRaster to be processed

- scale:

  Optional character string for scaling the results; See Details

- mask:

  Optional `SpatRaster` or character string giving the filepath to a
  raster that should be used to mask the output, e.g. a study area
  boundary

- python:

  Optional filepath to the preferred version of arcpy, passed to
  [`reticulate::use_python`](https://rstudio.github.io/reticulate/reference/use_python.html);
  See details.

- dir, SDM, landscape_name:

  Optional; Character strings defining the filepath where output raster
  should be written (`dir/SDM/landscape_name`)

- filename:

  name of the output raster, including file extension; default is
  'droost_km.tif', the name of the predictor required by the waterbird
  models

- ...:

  Additional arguments passed to
  [`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html)

## Value

SpatRaster

## Details

This function calls the `dist_stats.py` script to calculate the
Euclidean distance for all cells in the input raster without a value to
the nearest cell with a value (e.g., for calculating distance to a crane
roost or a stream). Optionally, results can be scaled, masked, and
written to `dir/SDM/landscape_name/filename`.

Currently supported scale options include: `km` to divide the results by
1000 and return distances in kilometers or `sqrt` to take the square
root of the results.

The parameters `dir`, `SDM`, `landscape_name`, and `filename` are passed
to [`file.path()`](https://rdrr.io/r/base/file.path.html) to contstruct
filenames passed to
[`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html).
If multiple values for one of these character strings is provided, the
output raster will be written to more than one directory, e.g. if needed
as a predictor for multiple SDMs or landscape names.

This function relies on the availability of `arcpy` and Spatial Analyst
extensions. An attempt will be made to load these the first time this
function (or
[`python_focal_stats()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/python_focal_stats.md))
is called in each session, and by default will look here:
`C:/Program Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3/python.exe`;
use the `python` argument to specify a different pathway.

## Examples

``` r
# See vignette
```
