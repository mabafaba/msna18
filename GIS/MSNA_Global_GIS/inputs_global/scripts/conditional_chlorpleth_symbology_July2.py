#this will be the script that allows the model to branch and choose symbology

import arcpy
import pandas
from pandas import DataFrame

#need to define several parameters to be input
# in this example- this should be
#get the value of the input parameter
input_argument = arcpy.GetParameterAsText(0)
field_name = arcpy.GetParameterAsText(1)
#define layer style files as variables

chloropleth_2_colors = "C:\\Users\\Milan\\Documents\\Impact_GIS\\02_MSNA_2018\\Global_Data_Analysis_Team\\MSNA_Global_GIS\\inputs_global\\style\\lyrfiles\\clpth_2colors_diverging_rdbu.lyr"
chloropleth_3_colors = "C:\\Users\\Milan\\Documents\\Impact_GIS\\02_MSNA_2018\\Global_Data_Analysis_Team\\MSNA_Global_GIS\\inputs_global\\style\\lyrfiles\\clpth_3colors_diverging_rdbu.lyr"
chloropleth_4_colors = "C:\\Users\\Milan\\Documents\\Impact_GIS\\02_MSNA_2018\\Global_Data_Analysis_Team\\MSNA_Global_GIS\\inputs_global\\style\\lyrfiles\\clpth_4colors_diverging_rdbu.lyr"
chloropleth_5_colors = "C:\\Users\\Milan\\Documents\\Impact_GIS\\02_MSNA_2018\\Global_Data_Analysis_Team\\MSNA_Global_GIS\\inputs_global\\style\\lyrfiles\\clpth_5colors_diverging_rdbu.lyr"
chloropleth_6_colors = "C:\\Users\\Milan\\Documents\\Impact_GIS\\02_MSNA_2018\\Global_Data_Analysis_Team\\MSNA_Global_GIS\\inputs_global\\style\\lyrfiles\\clpth_6colors_diverging_rdbu.lyr"
chloropleth_7_colors = "C:\\Users\\Milan\\Documents\\Impact_GIS\\02_MSNA_2018\\Global_Data_Analysis_Team\\MSNA_Global_GIS\\inputs_global\\style\\lyrfiles\\clpth_7colors_diverging_rdbu.lyr"

#this function turns arc layer into pandas data frame
def feature_class_to_pandas_data_frame(feature_class, field_list):
    """
    Load data into a Pandas Data Frame for subsequent analysis.
    :param feature_class: Input ArcGIS Feature Class.
    :param field_list: Fields for input.
    :return: Pandas DataFrame object.
    """
    return DataFrame(
        arcpy.da.FeatureClassToNumPyArray(
            in_table=feature_class,
            field_names=field_list,
            skip_nulls=False,
            null_value=-99999
        )
    )
#run function
ans_df = feature_class_to_pandas_data_frame(input_argument,field_name)
#get the number of unique values in the answer column (# of colors for map)
num_unique = len(ans_df[field_name].unique())

#now a series of if else statements to define different branches for model
if num_unique == 2:
    arcpy.ApplySymbologyFromLayer_management(input_argument, chloropleth_2_colors )
    arcpy.AddMessage("you have 2 unique number 1 answer choices")
elif num_unique == 3:
    arcpy.ApplySymbologyFromLayer_management(input_argument, chloropleth_3_colors)
    arcpy.AddMessage("you have 3  unique number 1 answer choices")
elif num_unique == 4:
    arcpy.ApplySymbologyFromLayer_management(input_argument, chloropleth_4_colors)
    arcpy.AddMessage("you have 4  unique number 1 answer choices")
elif num_unique == 5:
    arcpy.ApplySymbologyFromLayer_management(input_argument, chloropleth_5_colors)
    arcpy.AddMessage("you have 5  unique number 1 answer choices")
elif num_unique == 6:
    arcpy.ApplySymbologyFromLayer_management(input_argument, chloropleth_6_colors)
    arcpy.AddMessage("you have 6  unique number 1 answer choices")
elif num_unique == 7:
    arcpy.ApplySymbologyFromLayer_management(input_argument, chloropleth_7_colors)
    arcpy.AddMessage("you have 7  unique number 1 answer choices")
elif num_unique >= 8:
    sys.exit(arcpy.AddError("Error: you have more than 7 colors"))
elif num_unique<2:
    arcpy.AddError("Error: Why would you want a chloropleth map with only 1 color- that wouldn't really be a chloropleth now would it")
else:
    arcpy.AddError("Error: Somethings wrong with your data")
    

