# Metrics by land cover class and benefits category

Estimated mean value and standard error for each metric and land cover
class, used within the `DeltaMultipleBenefits` framework to estimate the
total score for a given landscape and the net change expected to result
from a change between landscapes.

## Usage

``` r
metrics
```

## Format

### `metrics` A data frame with 276 rows and 7 columns:

- METRIC_CATEGORY:

  Character string; metrics are organized into groups of benefits
  categories

- METRIC:

  Character string; metric name

- UNIT:

  Character string; units in which the metric is presented, usually per
  hectare

- CODE_NAME:

  Text string joining major land cover classes to subclasses with a '\_'

- LABEL:

  Default value for labels used to represent CODE_NAME in maps and plots

- SCORE_MEAN:

  Numeric; mean value for each metric and land cover class

- SCORE_SE:

  Numeric; standard error of the mean

## Source

Dybala et al. 2025 (https://doi.org/10.15447/sfews.2025v23iss2art2)
