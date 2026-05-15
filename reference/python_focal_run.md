# Run focal statistics via Python

Wrapper function to call a python script for calculating focal
statistics on landscape rasters. On first call this function imports the
arcpy module and Spatial Analyst extensions then sources the Python
script "focal_stats.py".

## Usage

``` r
python_focal_run(
  pathin,
  landscape_name,
  SDM,
  regex = NULL,
  scale,
  fun = "SUM",
  pathout,
  python = NULL
)
```

## Arguments

- pathin, SDM, landscape_name:

  Character strings defining the filepath (`pathin/SDM/landscape_name`)
  containing input rasters to be processed, such as those created from
  running
  [`python_focal_prep()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/python_focal_prep.md)

- regex:

  Optional regular expression to process only a subset of the rasters in
  `pathin/SDM/landscape_name`

- scale:

  String representing the buffer size (in m) within which focal stats
  are calculated

- fun:

  Function to summarize focal statistics: `'SUM'` or `'MEAN'`

- pathout:

  Filepath for the directory where output rasters should be written

- python:

  Optional filepath to the preferred version of arcpy, passed to
  [`reticulate::use_python`](https://rstudio.github.io/reticulate/reference/use_python.html).
  See details.

## Value

Nothing returned to R environment. Writes rasters to `pathout` for each
land cover class.

## Details

This function calls the `focal_stats.py` script to summarize cell values
for the input raster within a buffer distance defined by `scale`.
Summary functions may include `'SUM'` or `'MEAN'`. The default of
`fun = SUM'` is intended to be called only after first running
[`python_focal_prep()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/python_focal_prep.md),
which prepares rasters representing the presence/absence of individual
land cover classes, as defined by each set of species distribution
models, and allows 'SUM' to effectively count the number of pixels of
each land cover class within a given distance. This function can also be
used with `fun = 'MEAN'` to estimate the mean probability of open water
for a given land cover class within a given distance (i.e. \_pfld
predictors for waterbird models). See vignette.

*Important:* This function requires the availability of arcpy and
Spatial Analyst extensions. Use the `python` argument to specify the
local pathway to arcpy, particularly if other versions of Python are
installed. For example: 'C:/Program
Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3/python.exe'. Note that
the python argument is applied only on the first use within each
session, and must be repeated in each session.

## See also

[`python_focal_prep()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/python_focal_prep.md),
[`python_focal_finalize()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/python_focal_finalize.md)

## Examples

``` r
# See vignette
```
