# Summarize total metric scores

Calculate the landscape-level sum of metrics for a set of landscape
rasters

## Usage

``` r
sum_metrics(metricdat, areadat)
```

## Arguments

- metricdat:

  tibble; See Details

- areadat:

  tibble; See Details

## Value

tibble

## Details

This function will combine the landscape-specific estimates of the total
area of each land cover class provided by `areadat` (optionally also by
zone; see
[`sum_landcover()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/sum_landcover.md))
with per-unit-area metrics associated with each land cover class as
provided in `metricdat`. The two data sets are joined by any fields in
common, which should at minimum include a field containing the land
cover classes.

For most metrics, scores are calculated by multiplying the total area by
the corresponding per-unit-area metric and summing over the entire
landscape. However, for Annual Wages associated with agricultural jobs,
a weighted average is instead produced, based on the average wage for an
agricultural job associated with each land cover class and the
proportion of the landscape that has an associated wage (i.e., only crop
classes) that is made up by that land cover class. In addition, for the
metrics representing the Climate Change Resilience category, the overall
landscape average score is produced.

This function expects `metricdat` to contain the following fields:

- class: a character field containing the land cover classes; may be
  called anything, but should match with a corresponding field in
  `areadat`

- METRIC: character field used to define specific metrics; expects a
  METRIC called "Annual Wages"

- SCORE_MEAN, SCORE_SE: numeric fields containing the specific values
  for each METRIC and an estimate of uncertainty

- METRIC_CATEGORY, UNIT: optional additional character fields useful for
  grouping METRICS and tracking units; may contain anything, will be
  retained in output. If UNIT is present, "/ha" will be removed from the
  results before returning, as a reminder that the resulting total
  scores are no longer per-hectare.

This function also expects `areadat` to contain the following fields:

- scenario: character field used to identify the name of the landscape
  being examined

- class: as above, a character field containing the land cover classes;
  may be called anything, but should match with a corresponding field in
  `metricdat`

- area: numeric field containing the total area of each land cover
  class. Take care that the units in which the area field was calculated
  correspond to the UNITs by which the per-unit-area METRICS in
  `metricdat` were defined (often ha). (See
  [`sum_landcover()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/sum_landcover.md).)

- ZONE: optional character field identifying the name of a zone within
  which the area of land covers was estimated, such as by running
  [`sum_landcover()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/sum_landcover.md)
  with `zones` provided; if ZONE is present, the output will be
  summarized by zone.

## See also

[`sum_landcover()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/sum_landcover.md)

## Examples

``` r
# See vignette
```
