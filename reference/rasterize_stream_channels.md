# Rasterize stream channels

Convert stream channel line data to rasters for use with "tima" models

## Usage

``` r
rasterize_stream_channels(
  x,
  template,
  min_length = NULL,
  SDM = "tima",
  landscape_name = "baseline",
  dir = NULL,
  filename = "CHAN.tif",
  ...
)
```

## Arguments

- x:

  object of class sf, or file path to an object than can be read by `sf`

- template:

  SpatRaster to be used as a template for rasterizing stream channel
  data

- min_length:

  Optional; minimum length of stream channel to consider

- SDM:

  The name of intended species distribution model; by default `"tima"`

- landscape_name:

  The name of the landscape scenario represented; by default
  `"baseline"`

- dir:

  Optional string representing directory passed to
  [`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html),
  as (`dir/SDM/landscape_name`). See Details.

- filename:

  The filename of the output raster; by default "CHAN.tif"

- ...:

  Additional arguments passed to
  [`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html)

## Value

SpatRaster

## Details

This function provides support for converting the California Aquatic
Resources Inventory (CARI) Streams (lines) data set to a rasterized
representation for calculating proportion cover on multiple spatial
scales. It uses the same criteria used in the original analysis, lines
labeled as 'not shown' or 'Fluvial Subsurface' were excluded. It also
provides an option to exclude lines shorter than a minimum length (in
meters), which was set to 1 in the original analysis.

The CARI data are freely available for download from
[SFEI](https://www.sfei.org/data/california-aquatic-resource-inventory-cari-gis-data)
and
[CDFW](https://filelib.wildlife.ca.gov/Public/BDB/GIS/BIOS/metadata/DS2836.html).

By default, this layer will be assumed applicable to the `tima` SDM
models and assigned as the "baseline" representation of stream channels
unless the `SDM` or `landscape_name` parameters are otherwise specified.
This function can be re-run for alternative scenarios (or the resulting
files simply copied to the appropriate directory) if stream channel
locations and densities will not change in the alternative scenario.
Otherwise, the result of this function can be manipulated to represent
changes to channel locations under alternative scenarios.
