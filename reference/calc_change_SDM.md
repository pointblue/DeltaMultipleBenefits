# Calculate the difference between predictions of species presence for a baseline and scenario landscape

Calculate the difference between predictions of species presence for a
baseline and scenario landscape

## Usage

``` r
calc_change_SDM(
  pathin,
  SDM,
  baseline_name,
  scenario_name,
  pathout,
  overwrite = FALSE,
  differentiate = TRUE
)
```

## Arguments

- pathin, SDM:

  Character strings defining the filepath (`pathin/SDM`) containing the
  predicted probability of presence resulting from each distribution
  model, such as those created from running
  [`fit_SDM()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/fit_SDM.md)
  and
  [`transform_SDM()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/transform_SDM.md)

- baseline_name:

  Character string defining the filepath (`pathin/SDM/baseline_name`)
  containing the predictions for a baseline landscape

- scenario_name:

  Character string defining the filepath (`pathin/SDM/scenario_name`)
  containing the predictions for a scenario landscape

- pathout:

  Character string defining the filepath (`pathout/SDM/scenario_name`)
  where the results will be written

- overwrite:

  Logical; passed to
  [`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html);
  default `FALSE`

- differentiate:

  Logical; if `TRUE`, locations where the predicted value is zero in
  both landscapes will be converted to NA

## Value

Nothing; all output written to `pathout/SDM/scenario_name`

## Details

This function is designed to handle predictions for multiple species
provided as rasters in the `baseline` and `scenario` directories,
matched by name. The difference is calculated as the predicted value for
the scenario minus the predicted value for the baseline, such that
positive values in the result represent an increased probability of
presence, and negative values represent a reduced probability of
presence.

If `differentiate = TRUE`, the function is also designed to distinguish
between two types of zero values in the result: locations where the
predicted value is the same in both landscapes (which will retain a
value of zero difference), and locations where the predicted value is
zero in both landscapes (which will be converted to NA).

## Examples

``` r
# See vignette
```
