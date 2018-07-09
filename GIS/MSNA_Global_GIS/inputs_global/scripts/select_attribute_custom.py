import arcpy
from arcpy.analysis import *

#Define input/output
input = arcpy.GetParameterAsText(0)
fieldName = arcpy.GetParameterAsText(1)
values = arcpy.GetParameterAsText(2)
output = arcpy.GetParameterAsText(3)

#Build the where clause
queryJoin = "' OR " + fieldName + " = '"
whereClause = fieldName + " = '" + queryJoin.join(values) + "'"

Select(input, output, whereClause)
