# Summarize net change between scenario and baseline landscapes

Calculate the difference in the landscape-level metric totals between a
baseline landscape and one or more scenario landscapes

## Usage

``` r
sum_change(dat, k = 2)
```

## Arguments

- dat:

  tibble; see Details

- k:

  coverage factor; see Details

## Value

tibble containing fields `SCENARIO_VALUE`, `BASELINE_VALUE`,
`net_change`, and optionally `SCENARIO_SE`, `BASELINE_SE`,
`net_change_se`, `U`, `lcl`, `ucl`, and `z`; see Details

## Details

This function expects `dat` to contain the following fields:

- `scenario`: a character field used to identify the name of the
  landscape being examined; one of these must be called `'baseline'` and
  all others will be treated as alternate scenarios for comparison with
  the baseline

- either `area` or `SCORE_TOTAL`: numeric fields representing the total
  landscape-level values for each metric, such as those produced by
  [`sum_landcover()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/sum_landcover.md)
  or
  [`sum_metrics()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/sum_metrics.md).

The scores for each scenario are aligned with corresponding scores for
the baseline landscape based on any other common fields (e.g. `ZONE`,
`METRIC_CATEGORY`) and the net difference is calculated as the scenario
score minus the baseline score. The function returns the original
baseline and scenario total landscape scores for each metric and
scenario (renamed as `BASELINE` and `SCENARIO`), along with
`net_change`.

If `SCORE_TOTAL_SE` is also provided in `dat`, representing the
uncertainty in the `SCORE_TOTAL`, the uncertainty in the difference
(`net_change_se`) is also calculated as:
`sqrt(BASELINE_SE^2 + SCENARIO_SE^2)` where `BASELINE_SE` and
`SCENARIO_SE` represent the `SCORE_TOTAL_SE` for the baseline and
scenario landscapes, respectively. In addition, the coverage factor `k`
is used to estimate expanded uncertainty (`U`), or the interval within
which a large fraction of the distribution of values could be reasonably
expected. The appropriate value for `k` depends on the level of
confidence required, the number of observations on which the uncertainty
is based, and any knowledge of the underlying distributions of the
estimates. Where the distributions concerned are normal, and for most
purposes, a value of 2 is recommended to give an interval containing
approximately 95% of the distribution of values. In this case, the
function also returns the original uncertainty estimates for the
baseline and scenario (renamed as `BASELINE_SE` and `SCENARIO_SE`),
`net_change_se`, the expanded uncertainty estimate `U` and corresponding
`lcl` and `ucl`, as well as a `z` score equal to
`abs(net_change/net_change_se)`.

## See also

[`sum_habitat()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/sum_habitat.md),
[`sum_metrics()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/sum_metrics.md)

## Examples

``` r
# See vignette
```
