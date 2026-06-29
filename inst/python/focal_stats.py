# import arcpy
# from arcpy import env
# from arcpy.sa import *
import os
import arcpy
from arcpy.sa import FocalStatistics, NbrCircle, SetNull, IsNull

def focal_stats(fullpathin, fullpathout, buffer, suffix, fun = 'SUM', 
                regex = '*', overwrite=True, mask_raster=None):
  # Set environment settings
  arcpy.env.workspace = os.path.abspath(fullpathin)
  #print("workspace:", arcpy.env.workspace)
  
  # Get a list of the matching rasters in the workspace
  rasters = arcpy.ListRasters(regex)
  #print("rasters:", rasters)
  if not rasters:
    raise RuntimeError("No rasters found")
  
  fullpathout = os.path.abspath(fullpathout)
  
  # print("mask_raster repr:", repr(mask_raster), "type:", type(mask_raster))
  
  # Loop through the list of rasters
  for inRaster in rasters:
    base, ext = os.path.splitext(inRaster)
    out_name = base + suffix + ext
    outRaster = os.path.join(fullpathout, out_name)
    #print("will save to:", outRaster)
    
    if overwrite and arcpy.Exists(outRaster):
      print("Deleting existing layer:", inRaster)
      arcpy.Delete_management(outRaster)
      # try:
      #   arcpy.Delete_management(outRaster)
      #   deleted = True
      # except Exception:
      #   #also try removing as a regular file if needed
      #   try:
      #     os.remove(outRaster)
      #     deleted = True
      #   except Exception:
      #     deleted = False
      #     
      # if not deleted:
      #   arcpy.AddWarning(f"Could not delete existing output: {outRaster}")
      #   print(f"WARNING: Could not delete existing output: {outRaster}")
      #   continue # skip to next
    
    # Process focal stats ("DATA" argument means NA values will be dropped)
    outFocalStat = FocalStatistics(inRaster, NbrCircle(buffer, "Map"), fun, "DATA")
    
    # apply mask if provided
    if mask_raster:
      print("Attempting to mask")
      #masked = ExtractByMask(outFocalStat, mask_raster) # crops to extent of mask

      # keep outFocalStat where mask is not null; set others to NoData
      masked = SetNull(IsNull(mask_raster), outFocalStat)
      save_obj = masked
    else:
      save_obj = outFocalStat

    try:
      save_obj.save(outRaster)
      #outFocalStat.save(outRaster)
    except Exception as e:
      print("SAVE ERROR:", e)
      raise
    #print("saved exists arcpy:", arcpy.Exists(outRaster), "os:", os.path.exists(outRaster))
