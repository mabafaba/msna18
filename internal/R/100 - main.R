message(("loading dependencies.."))
# clear/create folders
source("./internal/R/110 - initialise_folders.R",local = T)
#load dependencies
suppressMessages(source("./internal/R/120 - dependencies.R"))
# this installed and loaded all external packages and local scripts.
clear_console()

# set up log file:
.log<-list();.logfile<-"./output/log/log.txt";.clearlog()


source("./internal/R/130 - load_input.R",local = T)
# this created following objects:
# data  # questionnaire  # data_parameters # analysis_plan_user
# cluster_formula()  # weights_of() # question_is_skipped()
# question_is_numeric()  # question_is_categorical()  # question_is_select_one()  # question_is_select_multiple()  # question_variable_type()
# question_get_choice_labels()  # question_get_question_label()

source("./internal/R/140 - composite_indicators.R", local = T)
# ...
source("./internal/R/150 - analysis.R", local = T)
#...

source("./internal/R/160 - output.R", local = T)
#
