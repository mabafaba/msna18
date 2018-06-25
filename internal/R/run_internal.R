source("./internal/R/dependencies.R")
source("./internal/R/recoding.R")
source("./internal/R/load_analysis_definitions.R")
source("./internal/R/composite_indicator_weighted_count.R")

data<-read.csv("./internal/input_files/data.csv")
ci_weighted_count_def<-load_composite_indicator_definition_weighted_count()
data_with_composite_indicators<-add_variable_indicators_weighted_count(data,ci_weighted_count_def)
write.csv(data_with_composite_indicators,"./output/modified_data/data_w_weighted_counts.csv")
message(paste0("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\nsee '",getwd(),"/output' for your results!"))

rm(list=ls())
