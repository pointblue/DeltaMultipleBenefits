#' Classify landcover
#' @rdname classify_landcover
#' @param x object of class sf, sfc, sfg, or SpatRaster
#' @param ... Unused
#' @export

classify_landcover <- function(x, ...) UseMethod("classify_landcover")

#' Classify landcover data for simple features object
#'
#' Prepare a land cover data set for use with the other functions in this
#' package by aligning the land cover classifications with those required by the
#' [metrics] data as provided in the [key] and corresponding predictors required
#' for fitting species distribution models.
#'
#' This function provides support for cross-walking polygon-based vegetation
#' data, such as the "Habitat_types_modern_forLSPT" layer published in the Landscape
#' Scenario Planning Tool v3 (LSPT), to the land cover classes required for use
#' with this package. Ultimately, this function may be updated to extend support
#' to other land cover data sources, but it currently relies on the fields
#' provided in that layer, especially: `Habitat_Type`, `Source_classification`,
#' `Crop2016`, and, `Tidal`.
#'
#' The function returns an sf object with additional fields:
#' * `CODE_NAME`: most specific land cover class designation, matching those provided in the [key]
#' * `CODE_NUM`: corresponding code number, matching those in the [key], for creating rasters
#' * `PREDICTOR_RIPARIAN`: corresponding predictor name used in the riparian landbird SDMs
#' * `PREDICTOR_WATERBIRD_FALL`: corresponding predictor name used in the waterbird SDMs for the fall season
#' * `PREDICTOR_WATERBIRD_WIN`: corresponding predictor name used in the waterbird SDMs for the winter season
#' * `PREDICTOR_TIMA`: corresponding predictor name used in the tidal marsh bird SDMs
#'
#' The `CODE_NAME` field can be rasterized for further analysis with the
#' [metrics] data and converted to predictors for fitting the SDMs by calling
#' this function again on the raster (see [classify_landcover.SpatRaster()].
#' Alternatively, each predictor field can be rasterized directly from this
#' output. The results should be mostly identical, except for a few special
#' cases for the riparian predictors; see [predictors_riparian] for more
#' information.
#'
#' @param x object of class sf, sfc or sfg
#' @param source character string indicating the source of the land cover
#'   polygons; currently only "LSPT" is supported
#' @param ... Unused
#' @method classify_landcover sf
#'
#' @export
#' @seealso [key], [predictors_riparian], [predictors_waterbird_win],
#'   [predictors_waterbird_fall], [predictors_tima]
#'
#' @importFrom utils data

classify_landcover.sf <- function(x, source = 'LSPT', ...) {
  if (!inherits(x, "sf")) stop("x must be a simple features object")
  if (source == 'LSPT') {
    if (!all(c('Habitat_Type', 'Source_classification', 'Crop2016') %in% names(x))) {
      stop('expect x to contain fields named: "Habitat_Type", "Source_classification", and "Crop2016"')
    } else {
      res = map_habitat_simple(x) |>
        define_riparian_subclasses() |>
        define_wetland_subclasses() |>
        define_perennial_crops() |>
        define_annual_crops() |>
        handle_missing_crops() |>
        handle_water_barren() |>
        add_code_names() |> # including tidal status
        # add SDM predictors
        add_riparian_predictors() |>
        add_waterbird_predictors() |>
        add_tima_predictors() |>
        dplyr::select(-.data$CLASS, -.data$TIDAL)
    }
  } else {
    stop('only LSPT is currently supported')
  }
  return(res)
}

#' Classify landcover for SpatRaster object
#'
#' Prepare for fitting SDMs by reclassifying an existing landscape raster
#' according to the classifications used by a specific set of species
#' distribution models (SDM).
#'
#' Calls on internal datasets to crosswalk from land cover classes listed in the
#' [key] to the predictors expected by the selected SDM group. The input raster
#' should already be encoded with the land cover classes listed in the [key]. To
#' help with creating such a raster, see [classify_landcover.sf()] to map land
#' cover polygons to the land cover classes in the [key].
#'
#' A warning is given if there are land cover classes present in the landscape
#' that do not map to any of the predictors for the selected SDM group, or if
#' there are land cover classes missing from the landscape that are expected by
#' the selected SDM group. These warnings may represent significant problems for
#' fitting SDMs and should be carefully reviewed.  In either case, it is
#' recommended to review the corresponding internal datasets
#' ([predictors_riparian], [predictors_waterbird_fall],
#' [predictors_waterbird_win], or [predictors_tima]) for the list of expected
#' predictors and how they map to land cover classes in the [key]. Check whether
#' the selected SDM group expects more specific land cover classes or
#' subclasses; the input raster may need to be reclassified before proceeding.
#'
#' @param x SpatRaster
#' @param SDM The name of intended species distribution model: `"riparian"`,
#'   `"waterbird_fall"`, `"waterbird_win"`, or `"tima"`
#' @param coltab logical; if TRUE add default color palette
#' @param verbose logical; if TRUE then print details associated with warning messages
#' @param ... Unused
#' @method classify_landcover SpatRaster
#'
#' @export
#' @examples
#' r <- terra::rast(matrix(sample(c(11,19,71,72,90), size = 100, replace = TRUE),
#'          ncol = 10, nrow = 10))
#' r <- suppressWarnings(classify_landcover(r, SDM = 'riparian'))


classify_landcover.SpatRaster <- function(x, SDM, coltab = TRUE, verbose = TRUE, ...) {
  if (!inherits(x, "SpatRaster")) stop("x must be a SpatRaster")
  levels(x) <- NULL
  landscape_vars = terra::freq(x, usenames = FALSE)

  if (SDM == 'riparian') {
    pred <- DeltaMultipleBenefits::predictors_riparian |>
      dplyr::select(-.data$NOTES)
  } else if (SDM == 'waterbird_fall') {
    pred <- DeltaMultipleBenefits::predictors_waterbird_fall
  } else if (SDM == 'waterbird_win') {
    pred <- DeltaMultipleBenefits::predictors_waterbird_win
  } else if (SDM == 'tima') {
    pred <- DeltaMultipleBenefits::predictors_tima
  }

  # main set of classifications:
  crosswalk = dplyr::full_join(landscape_vars, pred, by = c('value' = 'CODE_NUM'))

  # check for excluded land covers: (present in landscape but excluded from model)
  excluded = crosswalk |>
    dplyr::filter(is.na(.data$PREDICTOR_NUM) & .data$count > 0) |>
    dplyr::mutate(prop = .data$count / sum(crosswalk$count, na.rm = TRUE))

  if (nrow(excluded) > 0) {
    if (sum(excluded$prop) > 0.1) {
      warning(
        strwrap(
          prefix = " ", initial = "",
          "Extreme Caution Advised. Land cover classes representing a
          substantial proportion of the landscape are not represented by any
          of the predictors fot the selected SDM. Check input raster for errors."
        )
      )
    } else {
      warning(
        strwrap(
          prefix = " ", initial = "",
          "Caution Advised. Some land cover classes are not represented by any
          of the predictors for the selected SDM. Check input raster for errors."))
    }
    if (verbose) {
      print(excluded |> dplyr::select('CODE_NAME', 'count', 'prop'))
    }

  }

  r = terra::classify(
    x,
    rcl = crosswalk |>
      dplyr::select(from = 'value', to = 'PREDICTOR_NUM') |>
      tidyr::drop_na(tidyselect::any_of('to')) |> as.matrix(),
    others = NA)
  levels(r) <- crosswalk |>
    dplyr::select('PREDICTOR_NUM', 'PREDICTOR_NAME') |>
    tidyr::drop_na() |> dplyr::distinct() |>
    dplyr::arrange('PREDICTOR_NUM')
  if (coltab) {
    # add default color palette
    terra::coltab(r) <- pred |>
      dplyr::select('PREDICTOR_NUM', 'COLOR') |>
      tidyr::drop_na() |> dplyr::distinct() |> as.data.frame()
  }

  # check that all required predictors are accounted for
  pred_unique = unique(pred$PREDICTOR_NAME)
  included = terra::freq(r)$value
  if (!all(pred_unique %in% included)) {
    missing = pred_unique[!pred_unique %in% included]
    if (verbose) {
      cat(missing)
    }
    warning(
      strwrap(
        prefix = " ", initial = "",
        "Extreme Caution Advised. Land cover classes are missing from the
        input raster but are expected by the selected SDM. Check input raster
        for errors."))
  }
  return(r)
}

map_habitat_simple = function(x) {
  res = dplyr::mutate(
    x,
    CLASS = dplyr::case_when(
      Habitat_Type %in% c('stabilized interior dune vegetation', 'chaparral/scrub') ~ 'SCRUB',
      Habitat_Type == 'urban' ~ 'URBAN',
      Habitat_Type == 'vernal pool complex' ~ 'VERNAL_POOL',
      Habitat_Type == 'oak woodland/savanna' ~ 'WOODLAND',
      Habitat_Type == 'grassland' ~ 'GRASSLAND')
  )
  return(res)
}

define_riparian_subclasses = function(x) {
  res = dplyr::mutate(
    x,
    CLASS = dplyr::case_when(
      Habitat_Type == 'valley foothill riparian' &
        Source_classification == 'Populus fremontii' ~
        'RIPARIAN_FOREST_POFR',
      Habitat_Type == 'valley foothill riparian' &
        grepl('Quercus', Source_classification) ~
        'RIPARIAN_FOREST_QUER',
      Habitat_Type == 'valley foothill riparian' &
        Source_classification %in% c(
          'Aesculus californica',
          'Fraxinus latifolia',
          'Juglans hindsii and Hybrids',
          'Platanus racemosa',
          'Southwestern North American riparian evergreen and deciduous woodland',
          'Vancouverian riparian deciduous forest') ~
        'RIPARIAN_FOREST_MIXED',
      Habitat_Type == 'riparian scrub/shrub' &
        Source_classification %in% c(
          'Acer negundo', 'Alnus rhombifolia') ~
        'RIPARIAN_FOREST_MIXED',
      Habitat_Type == 'riparian scrub/shrub' &
        Source_classification %in% c(
          'Salix gooddingii',
          'Salix laevigata',
          'Salix laevigata / Salix lasiolepis') ~
        'RIPARIAN_FOREST_SALIX',
      Habitat_Type %in% c('valley foothill riparian',
                          'riparian scrub/shrub') &
        Source_classification %in% c(
          'Rubus armeniacus',
          'Rubus armeniacus - Sesbania punicea - Ficus carica',
          'Southwestern North American introduced riparian scrub',
          'Tamarix spp.') ~
        'RIPARIAN_SCRUB_INTRODUCED',
      Habitat_Type %in% c('riparian scrub/shrub', 'willow thicket') &
        Source_classification %in% c(
          'Baccharis pilularis', 'Baccharis pilularis / Annual Grass-Herb',
          'Baccharis salicifolia', 'Carex barbarae', 'Cephalanthus occidentalis',
          'Cornus sericea', 'Rosa californica',
          'Rosa californica - Baccharis pilularis', 'Sambucus nigra',
          'Southwestern North American riparian/wash scrub',
          'Vitis californica') ~
        'RIPARIAN_SCRUB_MIXED',
      Habitat_Type %in% c('riparian scrub/shrub', 'willow thicket') &
        Source_classification %in% c(
          'Salix exigua',  'Salix lasiolepis', 'Salix lucida') ~ 'RIPARIAN_SCRUB_SALIX',
      Habitat_Type == 'valley foothill riparian' &
        Source_classification == 'VRI' &
        Crop2016 == 'Alfalfa and Alfalfa Mixtures' ~ 'PASTURE_ALFALFA', # ONE EXCEPTION (NOT MEANT TO APPLY TO BUFFER ZONE)
      Habitat_Type == 'valley foothill riparian' &
        Source_classification == 'VRI' &
        Crop2016 == 'Miscellaneous Truck Crops' ~ 'ROW', # ONE EXCEPTION
      Habitat_Type == 'valley foothill riparian' &
        Source_classification == 'VRI' ~ 'RIPARIAN', # ALL OTHER UNSPECIFIED/TINY SLIVERS
      TRUE ~ CLASS
    )
  )
  return(res)
}

define_wetland_subclasses = function(x) {
  res = dplyr::mutate(
    x,
    CLASS = dplyr::case_when(
      # WETLAND
      Habitat_Type %in% c('alkali seasonal wetland complex',
                          'emergent wetland') &
        Source_classification %in% c(
          'Atriplex prostrata - Cotula coronopifolia',
          'Cotula coronopifolia',
          'Sesuvium verrucosum',
          'Western North American disturbed alkaline marsh and meadow') ~
        'WETLAND_EMERGENT_DISTURBED',
      Habitat_Type == 'emergent wetland' &
        grepl('Spergularia', Source_classification) ~
        'WETLAND_EMERGENT_DISTURBED', # necessary due to strange encoding in the space
      Habitat_Type == 'alkali seasonal wetland complex' ~
        'WETLAND_ALKALI_SCRUB', # all alkali wetland complex
      Habitat_Type == 'emergent wetland' &
        Source_classification %in% c(
          'Arid West freshwater emergent marsh',
          'Arid West Freshwater Emergent Marsh',
          'Bolboschoenus maritimus',
          'Schoenoplectus (acutus, californicus)',
          'Schoenoplectus americanus',
          'Schoenoplectus americanus / Lepidium latifolium',
          'Schoenoplectus californicus - Schoenoplectus acutus',
          'Schoenoplectus californicus - Schoenoplectus acutus / Rosa californica',
          'Southwestern North American alkali marsh/seep vegetation',
          'Typha (angustifolia, domingensis, latifolia)') ~
        'WETLAND_TULE_CATTAIL',
      Habitat_Type == 'emergent wetland' &
        Source_classification %in% c(
          'Sarcocornia pacifica (Salicornia depressa)',
          'Sarcocornia pacifica - Cotula coronopifolia',
          'Sarcocornia pacifica - Crypsis schoenoides',
          'Sarcocornia pacifica - Sesuvium verrucosum',
          'Sarcocornia pacifica / annual grasses (Polypogon, Hordeum, Lolium)',
          'Sarcocornia pacifica Tidal') ~
        'WETLAND_PICKLEWEED',
      Habitat_Type == 'emergent wetland' &
        Source_classification %in% c(
          'Distichlis spicata',
          'Distichlis spicata - Cotula coronopifolia',
          'Distichlis spicata - Juncus arcticus var. balticus (J. arcticus var. mexicanus)',
          'Distichlis spicata - Sarcocornia pacifica',
          'Distichlis spicata - Schoenoplectus americanus',
          'Distichlis spicata - annual grasses') ~
        'WETLAND_SALTGRASS',
      Habitat_Type == 'emergent wetland' &
        Source_classification %in% c(
          'Arundo donax',
          'Phragmites australis',
          'Phragmites australis - Arundo donax') ~
        'WETLAND_PHRAGMITES_ARUNDO',
      Habitat_Type == 'emergent wetland' &
        Source_classification %in% c(
          'Frankenia salina',
          'Frankenia salina - Distichlis',
          'Grindelia (camporum, stricta)',
          'Temperate Pacific tidal salt and brackish meadow') ~
        'WETLAND_EMERGENT_OTHER',
      Habitat_Type == 'wet meadow/seasonal wetland' &
        Source_classification == 'Lepidium latifolium' ~
        'WETLAND_LEPIDIUM',
      Habitat_Type == 'wet meadow/seasonal wetland' &
        Source_classification %in% c(
          'Artemisia douglasiana',
          'Californian warm temperate marsh/seep',
          'Equisetum (arvense, variegatum, hyemale)',
          'Juncus arcticus (var. balticus, mexicanus)',
          'Leymus cinereus - Leymus triticoides') ~
        'WETLAND_MEADOW',
      Habitat_Type == 'wet meadow/seasonal wetland' &
        Source_classification %in% c(
          'Cynodon dactylon',
          'Cynodon dactylon - Crypsis spp. - Paspalum spp.',
          'Naturalized warm-temperate riparian and wetland',
          'Naturalized warm-temperate riparian and wetland group',
          'Polygonum lapathifolium - Xanthium strumarium') ~
        'WETLAND_MEADOW_NATURALIZED',
      Habitat_Type == 'emergent wetland' &
        Source_classification %in% c('FEW', 'SEW') ~ 'WETLAND', # OTHER UNSPECIFIED
      Habitat_Type == 'wet meadow/seasonal wetland' &
        Source_classification %in% c('Temperate and Boreal Salt Marsh') ~ 'WETLAND', # ALL OTHER UNSPECIFIED
      TRUE ~ CLASS
    )
  )
}

define_perennial_crops = function(x) {
  res = dplyr::mutate(
    x,
    CLASS = dplyr::case_when(
      Habitat_Type == 'agriculture/ruderal' &
        Crop2016 %in% c(
          'Citrus', 'Olives', 'Pistachios', 'Pomegranates',
          'Miscellaneous Subtropical Fruits') ~
        'ORCHARD_CITRUS&SUBTROPICAL',
      Habitat_Type == 'agriculture/ruderal' &
        Crop2016 %in% c(
          'Almonds', 'Apples', 'Cherries', 'Miscellaneous Deciduous',
          'Peaches/Nectarines', 'Pears', 'Plums, Prunes and Apricots',
          'Walnuts', 'Young Perennials') ~
        'ORCHARD_DECIDUOUS',
      Habitat_Type == 'agriculture/ruderal' &
        Crop2016 %in% c('Bush Berries', 'Grapes', 'Kiwis') ~
        'VINEYARD',

      TRUE ~ CLASS
    )
  )
  return(res)
}

define_annual_crops = function(x) {
  res = dplyr::mutate(
    x,
    CLASS = dplyr::case_when(
      Habitat_Type == 'agriculture/ruderal' &
        Crop2016 == 'Alfalfa and Alfalfa Mixtures' ~ 'PASTURE_ALFALFA',
      Habitat_Type == 'agriculture/ruderal' &
        Crop2016 == 'Mixed Pasture' ~ 'PASTURE_OTHER',
      Habitat_Type == 'agriculture/ruderal' &
        Crop2016 == 'Rice' ~ 'RICE',
      Habitat_Type == 'agriculture/ruderal' &
        Crop2016 == 'Wheat' ~ 'GRAIN&HAY_WHEAT',
      Habitat_Type == 'agriculture/ruderal' &
        Crop2016 %in% c(
          'Miscellaneous Grain and Hay',
          'Miscellaneous Grasses') ~ 'GRAIN&HAY_OTHER',
      Habitat_Type == 'agriculture/ruderal' &
        Crop2016 == 'Corn, Sorghum and Sudan' ~ 'FIELD_CORN',
      Habitat_Type == 'agriculture/ruderal' &
        Crop2016 %in% c(
          'Beans (Dry)', 'Safflower', 'Sunflowers') ~ 'FIELD_OTHER',
      Habitat_Type == 'agriculture/ruderal' &
        Crop2016 %in% c(
          'Carrots', 'Cole Crops', 'Cotton',
          'Flowers, Nursery and Christmas Tree Farms',
          'Lettuce/Leafy Greens', 'Melons, Squash and Cucumbers',
          'Miscellaneous Truck Crops', 'Onions and Garlic', 'Peppers',
          'Potatoes and Sweet Potatoes', 'Strawberries', 'Tomatoes') ~ 'ROW',
      TRUE ~ CLASS
    )
  )
  return(res)
}

handle_missing_crops = function(x) {
  # use source_classification when Crop2016 has no data
  res = dplyr::mutate(
    x,
    CLASS = dplyr::case_when(
      Habitat_Type == 'agriculture/ruderal' & (is.na(Crop2016) | Crop2016 == '') &
        Source_classification == 'DOR' ~ 'ORCHARD_DECIDUOUS',
      Habitat_Type == 'agriculture/ruderal' & (is.na(Crop2016) | Crop2016 == '') &
        Source_classification == 'VIN' ~ 'VINEYARD',
      Habitat_Type == 'agriculture/ruderal' & (is.na(Crop2016) | Crop2016 == '') &
        Source_classification == 'PAS' ~ 'PASTURE_OTHER',
      Habitat_Type == 'agriculture/ruderal' & (is.na(Crop2016) | Crop2016 == '') &
        Source_classification == 'RIC' ~ 'RICE',
      Habitat_Type == 'agriculture/ruderal' &
        (is.na(Crop2016) | Crop2016 == "" | Crop2016 == 'Managed Wetland') &
        Source_classification == 'Row and Close Grain Crop' ~ 'GRAIN&HAY_OTHER',
      Habitat_Type == 'agriculture/ruderal' & (is.na(Crop2016) | Crop2016 == '') &
        Source_classification == 'IRF' ~ 'FIELD_OTHER',
      # OTHERWISE, CONSIDER IDLE OR RUDERAL:
      Habitat_Type == 'agriculture/ruderal' &
        Crop2016 %in% c('Idle', 'Managed Wetland', 'Urban', '') & #not a specific crop
        Source_classification %in% c(# woody spp
          'EUC', 'Eucalyptus spp. - Ailanthus altissima - Robinia pseudoacacia',
          'Introduced North American Mediterranean woodland and forest',
          'Robinia pseudoacacia',
          'Temperate Tree Developed Vegetation') ~ 'RUDERAL_WOODY',
      Habitat_Type == 'valley foothill riparian' &
        Source_classification == 'Ailanthus altissima' ~ 'RUDERAL_WOODY', #exception
      Habitat_Type == 'agriculture/ruderal' &
        Crop2016 %in% c('Idle', 'Managed Wetland', 'Urban', '') & #not a specific crop
        Source_classification %in% c( # WEEDY SPP
          'Centaurea (solstitialis, melitensis)',
          'Conium maculatum - Foeniculum vulgare',
          'Cortaderia (jubata, selloana)',
          'Mesembryanthemum spp. - Carpobrotus spp.') ~ 'RUDERAL',
      Habitat_Type == 'agriculture/ruderal' &
        Crop2016 %in% c('Urban', 'Managed Wetland', '') & # NOT IDLE or a specific crop
        Source_classification %in% c(# PREVIOUSLY A CROP (OR ADJACENT)
          'AGR', 'Agriculture', 'CRP', 'IRF', 'PAS', 'RIC', 'DOR, EOR, VIN,') ~ 'RUDERAL',
      # if it's not mapped to a ruderal species and is called Idle, consider as Idle
      Habitat_Type == 'agriculture/ruderal' & Crop2016 == 'Idle' ~ 'AG_IDLE',
      TRUE ~ CLASS
    )
  )
  return(res)
}

handle_water_barren = function(x) {
  res = dplyr::mutate(
    x,
    CLASS = dplyr::case_when(
      Habitat_Type == 'barren' & Source_classification == 'Barren' &
        (Crop2016 == 'Managed Wetland' |
           Managed %in% c('waterfowl management', 'Other')) ~ 'WATER_SEASONAL_MUDFLAT',
      Habitat_Type == 'open water' &
        Source_classification %in% c(
          'Azolla (filliculoides, microphylla)',
          'Eichhornia crassipes',
          'Lemna (minor) and Relatives',
          'Ludwigia (hexapetala, peploides)',
          'Ludwigia (hexapetala, peploides) - Eichhornia crassipes',
          'Naturalized temperate Pacific freshwater vegetation',
          'Stuckenia pectinata',
          'Temperate freshwater floating mat') ~ 'WATER_FLOATING_VEG',
      Habitat_Type == 'open water'  ~ 'WATER', # all others
      Habitat_Type == 'barren' & Source_classification == 'BAR' &
        Managed == 'waterfowl management' ~ 'GRASSLAND', # ONE EXCEPTION IN LSPT
      Habitat_Type == 'barren' & Source_classification == 'BAR' ~ 'URBAN', # road edges
      Habitat_Type == 'barren' ~ 'BARREN', # ALL OTHER BARREN
      TRUE ~ CLASS)
  )
  return(res)
}

add_riparian_predictors = function(x) {
  # largely aligns with CLASS, but with a few distinctions because these were
  # originally based on CWHRCODE values that do not always agree with LSPT
  # Habitat_Type assignments; no tidal status information required
  # [NOTE: "PERM" predictor based on separate DU wetland layer - not obvious how to incorporate via LSPT]
  res = x |>
    dplyr::mutate(
      PREDICTOR_RIPARIAN = dplyr::case_when(
        # most are consistent 1:1 mapping from CLASS
        CLASS == 'RIPARIAN_FOREST_POFR' ~ 'POFR',
        CLASS == 'RIPARIAN_FOREST_QUER' ~ 'QULO', # MODEL MAY HAVE ORIGINALLY BEEN LIMITED TO "VRI" QULO, BUT SCENARIO ANALYSIS INCLUDED ALL QUERCUS
        CLASS == 'RIPARIAN_FOREST_SALIX' ~ 'SALIX',
        CLASS == 'RIPARIAN_FOREST_MIXED' ~ 'MIXEDFOREST',
        CLASS == 'RIPARIAN_SCRUB_SALIX' ~ 'SALIXSHRUB',
        CLASS == 'RIPARIAN_SCRUB_INTRODUCED' ~ 'INTROSCRUB', # BUT ALSO ADD PHRAG-ARUNDO
        CLASS %in% c(
          'RIPARIAN', 'RIPARIAN_UNSPECIFIED') ~ 'RIPARIAN', #ALL OTHERS UNSPECIFIED
        CLASS %in% c(
          'WETLAND_TULE_CATTAIL', 'WETLAND_PICKLEWEED', 'WETLAND_SALTGRASS',
          'WETLAND_EMERGENT_DISTURBED', 'WETLAND_EMERGENT_OTHER',
          'WETLAND_MEADOW', 'WETLAND', 'WETLAND_MANAGED',
          'WETLAND_MANAGED_PERENNIAL', 'WETLAND_MANAGED_SEASONAL',
          'WETLAND_OTHER') ~ 'WETLAND',
        CLASS %in% c(
          'AG_ANNUAL', 'GRAIN&HAY', 'GRAIN&HAY_WHEAT', 'GRAIN&HAY_OTHER',
          'FIELD', 'FIELD_CORN', 'FIELD_OTHER', 'ROW') ~ 'AG',
        CLASS == 'RICE' ~ 'RICE',
        CLASS %in% c(
          'GRASSLAND&PASTURE', 'AG_PASTURE', 'PASTURE_ALFALFA', 'PASTURE_OTHER',
          'GRASSLAND', 'VERNAL_POOL') ~ 'GRASSPAS',
        CLASS == 'AG_IDLE' ~ 'IDLE',

        CLASS == 'URBAN' ~ 'URBAN',
        CLASS %in% c('WOODLAND&SCRUB', 'WOODLAND', 'SCRUB', 'RUDERAL_WOODY') ~ 'WOODLAND&SCRUB',
        CLASS %in% c('WATER', 'WATER_FLOATING_VEG') ~ 'WATER',
        CLASS %in% c('BARREN', 'WATER_SEASONAL_MUDFLAT') ~ 'BARREN',

        # unique assignments accurate to original classification for riparian
        # models: (which were based on CWHRCODEs)
        CLASS == 'WETLAND_PHRAGMITES_ARUNDO' ~ 'INTROSCRUB',
        CLASS == 'WETLAND_ALKALI_SCRUB' ~ 'WOODLAND&SCRUB', #ASC
        CLASS == 'WETLAND_LEPIDIUM' ~ 'GRASSPAS', #WTM

        CLASS == 'RIPARIAN_SCRUB_MIXED' &
          Source_classification == 'Baccharis pilularis' ~ 'WOODLAND&SCRUB', #CSC
        CLASS == 'RIPARIAN_SCRUB_MIXED' &
          Source_classification == 'Carex barbarae' ~ 'WETLAND', #FEW
        CLASS == 'RIPARIAN_SCRUB_MIXED' ~ 'MIXEDSHRUB', # ALL OTHERS

        CLASS %in% 'WETLAND_MEADOW_NATURALIZED' &
          Source_classification %in% c(
            'Cynodon dactylon',
            'Cynodon dactylon - Crypsis spp. - Paspalum spp.') ~ 'GRASSPAS', # PAS, WTM
        CLASS == 'WETLAND_MEADOW_NATURALIZED' ~ 'WETLAND', # ALL OTHERS

        CLASS == 'VINEYARD' & Crop2016 == 'Bush Berries' ~ 'AG',
        CLASS %in% c(
          'ORCHARD_CITRUS&SUBTROPICAL', 'ORCHARD_DECIDUOUS', 'VINEYARD') ~
          'ORCHVIN',

        CLASS == 'RUDERAL' &
          Source_classification == 'Mesembryanthemum spp. - Carpobrotus spp.' ~
          'WOODLAND&SCRUB', #CSC
        CLASS == 'RUDERAL' ~ 'GRASSPAS') # ALL OTHERS
    )


  return(res)
}

add_waterbird_predictors = function(x) {
  # add predictors for both seasons; no tidal status information required
  res = x |>
    dplyr::mutate(
      PREDICTOR_WATERBIRD_FALL = dplyr::case_when(
        CLASS %in% c(
          'WETLAND_MANAGED_SEASONAL', 'WETLAND_MANAGED_PERENNIAL') ~ 'duwet', # though classification above will never produce these
        Crop2016 == 'Managed Wetland' ~ 'duwet', # aligns well with original DU wetland layer
        grepl('WETLAND', CLASS) ~ 'wet', # ALL OTHER WETLANDS
        CLASS %in% c(
          'AG_PERENNIAL', 'ORCHARD_DECIDUOUS', 'ORCHARD_CITRUS&SUBTROPICAL',
          'VINEYARD') ~ 'orch',
        CLASS %in% c('GRAIN&HAY', 'GRAIN&HAY_WHEAT', 'GRAIN&HAY_OTHER') ~ 'grain',
        CLASS == 'FIELD_CORN' ~ 'corn',
        CLASS == 'FIELD_OTHER' ~ 'field',
        CLASS == 'ROW' ~ 'row',
        CLASS == 'RICE' ~ 'rice',
        CLASS == 'AG_IDLE' ~ 'fal',
        CLASS == 'PASTURE_ALFALFA' ~ 'alf',
        CLASS == 'PASTURE_OTHER' ~ 'ip',
        CLASS %in% c('GRASSLAND', 'VERNAL_POOL', 'RUDERAL') ~ 'dryp',
        CLASS == 'URBAN' ~ 'dev',
        grepl('RIPARIAN', CLASS) ~ 'woodw',
        CLASS %in% c('WATER', 'WATER_FLOATING_VEG') ~ 'water',
        CLASS %in% c('BARREN', 'WATER_SEASONAL_MUDFLAT') ~ 'barren',
        CLASS %in% c('WOODLAND&SCRUB', 'WOODLAND', 'SCRUB', 'RUDERAL_WOODY') ~ 'for'),
      PREDICTOR_WATERBIRD_WIN = dplyr::case_when(
        CLASS == 'GRAIN&HAY_WHEAT' ~ 'ww',
        TRUE ~ PREDICTOR_WATERBIRD_FALL
      ))
  return(res)
}

add_tima_predictors = function(x) {
  res = x |> dplyr::mutate(
    PREDICTOR_TIMA = dplyr::case_when(
      CLASS %in% c('AG_PERENNIAL', 'ORCHARD_DECIDUOUS',
                   'ORCHARD_CITRUS&SUBTROPICAL', 'VINEYARD') ~ 'PNAG',
      CLASS %in% c('AG_ANNUAL', 'GRAIN&HAY', 'GRAIN&HAY_WHEAT',
                   'GRAIN&HAY_OTHER', 'FIELD', 'FIELD_CORN', 'FIELD_OTHER',
                   'ROW', 'AG_IDLE', 'GRASSLAND&PASTURE', 'AG_PASTURE',
                   'PASTURE_ALFALFA', 'PASTURE_OTHER', 'GRASSLAND', 'RUDERAL') ~ 'AGGRPAS',
      CLASS == 'RICE' ~ 'RICE',
      CLASS == 'URBAN' ~ 'URBN',
      CLASS == 'RIPARIAN_FOREST_POFR' ~ 'POFR',
      CLASS == 'RIPARIAN_FOREST_QUER' ~ 'QUER',
      CLASS == 'RIPARIAN_FOREST_SALIX' ~ 'SALF',
      CLASS == 'RIPARIAN_FOREST_MIXED' ~ 'MIXF',
      CLASS == 'RIPARIAN_SCRUB_INTRODUCED' ~ 'INTR',
      CLASS == 'RIPARIAN_SCRUB_SALIX' ~ 'SALS',
      CLASS == 'RIPARIAN_SCRUB_MIXED' ~ 'MIXS',
      CLASS == 'RIPARIAN' ~ 'RIPARIAN', #OTHER UNSPECIFIED
      CLASS == 'VERNAL_POOL' ~ 'VERP',
      CLASS %in% c('WATER', 'WATER_FLOATING_VEG', 'WATER_SEASONAL_MUDFLAT') ~ 'WATER',
      CLASS %in% c('WOODLAND&SCRUB', 'WOODLAND', 'SCRUB', 'RUDERAL_WOODY') ~ 'WOODY',
      CLASS == 'BARREN' ~ 'BARREN',
      CLASS == 'WETLAND_TULE_CATTAIL' ~ 'TULE',
      CLASS == 'WETLAND_PHRAGMITES_ARUNDO' ~ 'PHRA',
      CLASS %in% c('WETLAND_PICKLEWEED', 'WETLAND_SALTGRASS') ~ 'SALTPICK',
      CLASS %in% c('WETLAND_EMERGENT_OTHER', 'WETLAND_EMERGENT_DISTURBED') ~ 'EMER',
      CLASS %in% c('WETLAND_MEADOW', 'WETLAND_MEADOW_NATURALIZED') ~ 'MEAD',
      CLASS == 'WETLAND_LEPIDIUM' ~ 'LEPI',
      CLASS == 'WETLAND_ALKALI_SCRUB' ~ 'ALKA',
      CLASS == 'WETLAND' ~ 'WETLAND' # OTHER UNSPECIFIED
    )
  )
  return(res)
}

add_code_names = function(x) {
  res = x |> dplyr::mutate(
    TIDAL = dplyr::case_when(
      grepl('RIPARIAN|WETLAND', CLASS) & Tidal == 0 ~ 'NONTIDAL',
      grepl('RIPARIAN|WETLAND', CLASS) & Tidal == 1 ~ 'TIDAL'), # all others NA
    CODE_NAME = dplyr::case_when(
      !CLASS %in% c('RIPARIAN', 'WETLAND') &
        grepl('RIPARIAN|WETLAND', CLASS) ~ paste0(CLASS, '_', TIDAL),
      CLASS %in% c('RIPARIAN', 'WETLAND') ~ paste0(CLASS, '_UNSPECIFIED_', TIDAL),
      TRUE ~ CLASS)
  ) |>
    dplyr::left_join(
      DeltaMultipleBenefits::key |> dplyr::select(.data$CODE_NAME, .data$CODE_NUM),
      by = dplyr::join_by(CODE_NAME))
  return(res)
}
