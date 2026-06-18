# import arcpy
# from arcpy import env
# from arcpy.sa import *
import os
import arcpy
from arcpy.sa import FocalStatistics, NbrCircle

def focal_stats(fullpathin, fullpathout, buffer, suffix, fun = 'SUM', regex = '*'):
  # Set environment settings
  arcpy.env.workspace = fullpathin

  # Get a list of the matching rasters in the workspace
  rasters = arcpy.ListRasters(regex)
  
  if not os.path.exists(fullpathout):
    os.makedirs(fullpathout)
  
  # Set out folder
  #outFolder = fullpathout
  
  # Loop through the list of rasters
  for inRaster in rasters:
    base, ext = os.path.splitext(inRaster)
    out_name = base + suffix + ext
    outRaster = os.path.join(fullpathout, out_name)

    # Process focal stats ("DATA" argument means NA values will be dropped)
    outFocalStat = FocalStatistics(inRaster, NbrCircle(buffer, "Map"), fun, "DATA")
    outFocalStat.save(outRaster)
