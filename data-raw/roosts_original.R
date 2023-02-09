## code to prepare `roosts_original` dataset goes here

roosts_original = sf::read_sf('C:/Users/kdybala/OneDrive - Point Blue/Documents/A_Projects/delta_scenarios/GIS/original_source_data/Ivey/Select_recent_Roosts_Ivey_utm.shp')
roosts_original = dplyr::select(roosts_original, Roost_ID)
# roosts_original = terra::vect(roosts_original)

usethis::use_data(roosts_original, overwrite = TRUE)
