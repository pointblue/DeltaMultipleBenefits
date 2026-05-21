# Land cover classification scheme for the `DeltaMultipleBenefits` framework

Major land cover classes and subclasses designed to work with the
existing metrics and species distribution models within the
`DeltaMultipleBenefits` framework. It includes both natural and
agricultural land cover classes, and is organized hierarchically into
major land cover classes and subclasses. Also included are default
values for labels and color coding used in maps.

## Usage

``` r
key
```

## Format

### `key` A data frame with 81 rows and 7 columns:

- CODE_NUM:

  Numeric value used to encode rasters

- NAME_FULL:

  Text string joining major land cover classes to subclasses with a '\_'

- CLASS:

  Major land cover class grouping

- SUBCLASS:

  Land cover subclass

- DETAIL:

  Further land cover detail

- LABEL:

  Default value for labels used in maps and plots

- COLOR:

  Default hex color code used in maps

## Source

Dybala et al. 2023 (https://doi.org/10.15447/sfews.2023v21iss3art4);
Dybala et al. 2025 (https://doi.org/10.15447/sfews.2025v23iss2art2);
Dybala et al. In review
