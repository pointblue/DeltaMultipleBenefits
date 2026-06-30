# Changelog

## DeltaMultipleBenefits 2.0.0 - 2026-06-30

Major release including full support for Phase 2 models of tidal marsh
bird species distributions. Several functions have changed
substantially, breaking backwards compatibility.

#### Documentation

- [Supporting
  Information](https://pointblue.github.io/DeltaMultipleBenefits/news/articles/supporting_information.md):
  Added references for Phase II manuscript (in review) and published
  data sets
- [Overview](https://pointblue.github.io/DeltaMultipleBenefits/news/articles/overview.md):
  Updated information about the completed status of Phase II
- Vignette: Updated throughout to reference updated functions and Phase
  2 models

#### Data

- `key`: Simplified to full list of recognized land cover classes and
  subclasses
- `predictors_tima`, `predictors_riparian`, `predictors_waterbird_fall`,
  `predictors_waterbird_win`: added data sets laying out the crosswalk
  between land cover classes and subclasses in `key` with predictors
  expected by each set of SDMs

#### Functions

- `utils-py.R`: Added several Python helper functions to improved how
  Python is called and avoid importing arcpy and Spatial Analyst
  extensions repeatedly in the same session
- `reclassify_landcover`: Deprecated and replaced with
  `classify_landcover` and `create_predictor_stack`
- `classify_landcover`: Added function with methods for handling `sf` of
  `SpatRaster` input data; supports classification of land cover polygon
  data in alignment with `key` and classification of SpatRaster data to
  align with predictors required by each set of SDMs, respectively
- `create_predictor_stack`: Added internal function called by
  `python_focal_prep` to convert a classified raster into a stack of
  predictor variables
- `python_focal_finalize`: Deprecated and replaced with additional
  functionality in `focal_stats.py`
- `python_focal_stats`: Added wrapper to
  [`python_focal_run()`](https://pointblue.github.io/DeltaMultipleBenefits/reference/python_focal_run.md)
  to automatically handle selecting the correct spatial scales and
  summary statistics for each distribution model; also produces the
  appropriate predictor names and (experimentally) can mask the output
- `update_covertype`: Added support for processing SpatRaster with
  multiple layers
- `ALL`: replaced `pathout` with `dir` for specificity; added support
  for passing additional arguments to
  [`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html)
  where relevant; made writing SpatRasters to disk optional
- Added basic tests for `classify_landcover`, `create_predictor_stack`,
  and `python_focal_prep`

## DeltaMultipleBenefits 1.1.0 - 2026-05-14

Minor release including many updates and improvements to package
documentation, data, and functions. This release also includes partial
support for the distribution models developed for tidal marsh bird focal
species in Phase 2. Work in progress to test and finalize this support
before the next major release.

#### Documentation

- Added a `NEWS.md` file to track changes to the package.
- Updated references to relevant publications
- Added information about Phase II, focused on tidal wetland restoration
  and tidal marsh birds
- Added documentation to `fit_SDM` about the LANDCOVER predictors
  required to fit SDMs for tidal marsh focal species

#### Data

- `metrics`: Added Nitrogen loading metrics to allow calculating net
  change between scenarios
- `key`: Added tidal wetland vegetation subclasses to support
  distribution models for tidal marsh bird focal species

#### Functions

- Eliminated dependence on `magrittr` package
- `reclassify_landcover`: updated to support additional vegetation
  subclasses introduced in Phase II, including changes to how land
  covers are handled for the previous riparian and wetland SDMs; needs
  double-checking for final changes to tidal marsh SDMs since Nov 2025
- `python_focal_finalize`: added support for renaming focal stats as
  required to fit the tidal marsh SDMs
- `python_focal_prep`: updated documentation to indicate support for
  tidal marsh SDMs when it calls the `reclassify_landcover` function
- `sum_habitat`: Added support for specifying “tima” as a type of SDMs
  and general support for unrecognized SDMs; Added a more general
  `regex` arguments to allow selecting a subset of rasters rather than
  defaulting to all rasters in the directory
- `sum_landcover`: Generalized the “rollup” argument to only require
  wetland subclasses to start with “WETLAND” rather than
  “WETLAND_MANAGED”
- `transform_SDM`: Added a more general `regex` argument to allow
  selecting a subset of rasters rather than defaulting to all rasters in
  the directory
- `update_covertype`: Added support for generating the categorical
  LANDCOVER predictor as required to fit the tidal marsh SDMs; needs
  double-checking for final changes to tidal marsh SDMs since Nov 2025

## DeltaMultipleBenefits 1.0.0 - 2023-03-10

- First official release. Published via Zenodo:
  <https://doi.org/10.5281/zenodo.7718620>
