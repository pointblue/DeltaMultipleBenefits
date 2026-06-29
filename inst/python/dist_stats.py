# import arcpy
# from arcpy import env  
# from arcpy.sa import *  
import os
import arcpy
from arcpy.sa import EucDistance

def dist_stats(fullpathin, filename, fullpathout):
  
  # Set environment settings
  arcpy.env.workspace = os.path.abspath(fullpathin)
  #env.workspace = fullpathin
  
  # Calculate Euclidean Distance
  outEucDistance = EucDistance(in_source_data = filename)
  
  # Save
  fullpathout = os.path.abspath(fullpathout)
  outEucDistance.save(fullpathout)
