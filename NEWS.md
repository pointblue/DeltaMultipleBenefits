# Unreleased

- Improved how Python functions are called by adding several Python helper functions; avoids importing arcpy and Spatial Analyst extensions repeatedly in the same session.
- Added a test for the ability to load arcpy.
- `reclassify_landcover` and `python_focal_prep`: Streamlined process of crosswalking land cover classes to predictors used in each set of SDMs
- `key`: Simplified to full list of recognized land cover classes and subclasses
- `predictors_tima`, `predictors_riparian`, `predictors_waterbird_fall`, `predictors_waterbird_win`: added data sets laying out the crosswalk between land cover classes and subclasses in `key` with predictors expected by each set of SDMs

# DeltaMultipleBenefits 1.1.0 - 2026-05-14

Minor release including many updates and improvements to package documentation, data, and functions. This release also includes partial support for the distribution models developed for tidal marsh bird focal species in Phase 2. Work in progress to test and finalize this support before the next major release.

### Documentation

- Added a `NEWS.md` file to track changes to the package.
- Updated references to relevant publications
- Added information about Phase II, focused on tidal wetland restoration and tidal marsh birds
- Added documentation to `fit_SDM` about the LANDCOVER predictors required to fit SDMs for tidal marsh focal species

### Data

- `metrics`: Added Nitrogen loading metrics to allow calculating net change between scenarios
- `key`: Added tidal wetland vegetation subclasses to support distribution models for tidal marsh bird focal species

### Functions

- Eliminated dependence on `magrittr` package
- `reclassify_landcover`: updated to support additional vegetation subclasses introduced in Phase II, including changes to how land covers are handled for the previous riparian and wetland SDMs; needs double-checking for final changes to tidal marsh SDMs since Nov 2025
- `python_focal_finalize`: added support for renaming focal stats as required to fit the tidal marsh SDMs
- `python_focal_prep`: updated documentation to indicate support for tidal marsh SDMs when it calls the `reclassify_landcover` function
- `sum_habitat`: Added support for specifying "tima" as a type of SDMs and general support for unrecognized SDMs; Added a more general `regex` arguments to allow selecting a subset of rasters rather than defaulting to all rasters in the directory
- `sum_landcover`: Generalized the "rollup" argument to only require wetland subclasses to start with "WETLAND" rather than "WETLAND_MANAGED"
- `transform_SDM`: Added a more general `regex` argument to allow selecting a subset of rasters rather than defaulting to all rasters in the directory
- `update_covertype`: Added support for generating the categorical LANDCOVER predictor as required to fit the tidal marsh SDMs; needs double-checking for final changes to tidal marsh SDMs since Nov 2025

# DeltaMultipleBenefits 1.0.0 - 2023-03-10

- First official release. Published via Zenodo: <https://doi.org/10.5281/zenodo.7718620>
