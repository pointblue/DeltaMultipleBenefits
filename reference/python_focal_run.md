# Run focal statistics via Python

Internal function that checks for the availability of Python, imports
the arcpy module and Spatial Analyst extensions, then sources the Python
script "focal_stats.py".

## Usage

``` r
python_focal_run(
  pathin,
  landscape_name,
  SDM,
  regex = NULL,
  scale,
  suffix,
  fun = "MEAN",
  dir,
  python = NULL,
  overwrite = TRUE,
  mask = NULL
)
```

## Arguments

- pathin, SDM, landscape_name:

  Character strings defining the filepath (`pathin/SDM/landscape_name`)
  containing input rasters to be processed, such as those written from
  running
  [`python_focal_prep()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/python_focal_prep.md)

- regex:

  Optional regular expression to process only a subset of the rasters in
  `pathin/SDM/landscape_name`

- scale:

  String representing the buffer size (in m) within which focal stats
  are calculated

- fun:

  Function to summarize focal statistics: `'MEAN'` or `'SUM'`

- dir:

  Filepath for the directory where output rasters should be written (as
  `dir/SDM/landscape_name/scale`)

- python:

  Optional filepath to the preferred version of arcpy, passed to
  [`reticulate::use_python`](https://rstudio.github.io/reticulate/reference/use_python.html).
  See details.

- overwrite:

  logical; allow Python to overwrite output?

- mask:

  currently experimental

## Value

Nothing returned to R environment. Writes rasters to `pathout` for each
land cover class.

## Details

This function is primarily for internal use by
[`python_focal_stats()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/python_focal_stats.md)
to calculate focal statistics for each land cover predictor, passing the
appropriate moving window sizes and summary functions required by each
species distribution model. For each unique directory of land cover
predictors, this function calls the `focal_stats.py` script which reads
in all rasters in the directory (unless the `regex` argument is used)
and then summarizes cell values for each input raster within a buffer
distance defined by `scale`. Optionally, the results can be masked after
calculating focal statistics, such as to limit results to a study area.

Summary functions may include `'SUM'`, `'MEAN'`, or `'MAXIMUM'`. Note
that the MEAN of binary land cover presence data is equivalent to the
proportion cover of each land cover class within the buffer distance
while the SUM represents the count of pixels within the buffer distance.

## See also

[`python_focal_prep()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/python_focal_prep.md),
[`python_focal_stats()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/python_focal_stats.md)

## Examples

``` r
# See vignette
```
