# import arcpy
# from arcpy import env
# from arcpy.sa import *

def focal_stats(fullpathin, fullpathout, buffer, fun = 'SUM', regex = '*'):
  # Set environment settings
  env.workspace = fullpathin

  # Get a list of the matching rasters in the workspace
  rast = arcpy.ListRasters(regex)

  # Set out folder
  outFolder = fullpathout
  
  # Loop through the list of rasters
  for inRaster in rast:
    # Set the outputname for each output to be the same as the input
    outRaster = outFolder + "\\" + inRaster

    # Process focal stats ("DATA" argument means NA values will be dropped)
    outFocalStat = FocalStatistics(inRaster, NbrCircle(buffer, "Map"), fun, "DATA")
    outFocalStat.save(outRaster)
