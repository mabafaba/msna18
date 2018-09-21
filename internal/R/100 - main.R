# Main source file that calls all the source file for each step of the analysis. It will:
# 1- Delete (if any analysis was run before) and create all the folders for the outputs 
# 2- Load all the dependencies (functions, hypegrammaR, koboreadeR and install all packages needed)
# 3- Create a clean log file for records
# 4- Load and create all the input files
# 5- Create the composite indicators
# 6- Run the analysis
# 7- Present the results

# 1- Deletes and/or creates folders
source("./internal/R/110 - initialise_folders.R",local = T)

# 2- Loads and installs all external packages and local scripts.
message(("loading dependencies.."))
suppressMessages(source("./internal/R/120 - dependencies.R"))
clear_console()

# 3- Creates an empty log file
.log<-list();.logfile<-"./output/log/log.txt";.clearlog()

# 4- Loads the inputs files
source("./internal/R/130 - load_input.R",local = T)
# This created following objects:
# `data`, `data_parameters`, `questionnaire`, `analysis_plan_user`, `stratification_sf`, `cluster_sf`

# 5- Creates the composite indicators
source("./internal/R/140 - composite_indicators.R", local = T)
# This returned the dataset with the composite indicators and exported it to data_with_composite_indicators.csv

# 6- Runs the analysis - all the dirty work happens here!
source("./internal/R/150 - analysis.R", local = T)
# This created the `results` object. Everything is inside. All the analysis is already made, if there is an error 
#after this point, try again changing the outputs (barcharts, heatmaps, etc. ) you need in the analysis plan.

# 7- Creates all the output in an user friendly reading format
source("./internal/R/160 - output.R", local = T)
# This created:
# - all the charts
# - ResultsForfactsheets.RDS
# - allsummarystatistics.RDS
# - datamerge.csv
# - analysisplan_chart_filenames.csv
# - results.html

# You made it! Stay tuned for more! 
