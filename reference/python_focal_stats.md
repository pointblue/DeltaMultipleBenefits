# Run focal statistics via Python

Wrapper function to call a python script for calculating focal
statistics on land cover predictors for species distribution models. On
first call this function imports the arcpy module and Spatial Analyst
extensions then sources the Python script "focal_stats.py".

## Usage

``` r
python_focal_stats(
  SDM,
  pathin,
  dir,
  landscape_names,
  regex = NULL,
  python = NULL,
  overwrite = TRUE,
  mask = NULL
)
```

## Arguments

- pathin, SDM, landscape_names:

  Character strings defining the filepath (`pathin/SDM/landscape_names`)
  containing input rasters to be processed, such as those written from
  running
  [`python_focal_prep()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/python_focal_prep.md)

- dir:

  Filepath for the directory where output rasters should be written (as
  `dir/SDM/landscape_name/scale`)

- regex:

  Optional regular expression to process only a subset of the rasters in
  `pathin/SDM/landscape_name`

- python:

  Optional filepath to the preferred version of arcpy, passed to
  [`reticulate::use_python`](https://rstudio.github.io/reticulate/reference/use_python.html).
  See details.

- overwrite:

  logical; allow Python to overwrite existing output?

- mask:

  currently experimental

## Value

Nothing returned to R environment. Writes rasters to `pathout` for each
land cover class.

## Details

This function is designed to be called after running
[`classify_landcover.SpatRaster()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/classify_landcover.SpatRaster.md)
and
[`python_focal_prep()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/python_focal_prep.md),
and writing the resulting rasters representing land cover predictors to
`pathin/SDM/landscape_names`. For each raster in these directories, this
function calls inernal functions to calculate focal statistics required
for each species distribution model, on the appropriate spatial scales
and with the required summary statistics. The `regex` argument provides
options for processing only a subset of the rasters in the directory. If
`overwrite=TRUE` (the default), previously-created focal statistics will
be overwritten. If `mask` provides a filepath to a raster, the results
will be masked by this file before writing to file.

These calculations can be very slow, depending on the size and
resolution of the rasters, and relies on the availability of `arcpy` and
Spatial Analyst extensions for faster processing. An attempt will be
made to load these the first time this function is called in each
session, and by default will look here:
`C:/Program Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3/python.exe`;
use the `python` argument to specify a different pathway.

## See also

[`python_focal_prep()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/python_focal_prep.md),
[`fit_SDM()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/fit_SDM.md)

## Examples

``` r
# See vignette
```
