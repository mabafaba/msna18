# external packages to install/load:
.dependencies<-c("dplyr","questionr","data.table","tidyr","crayon","magrittr","rstudioapi","igraph")

source("./internal/R/composite_indicator_weighted_count.R")
source("./internal/R/survey_design.R")
source("./internal/R/recoding.R")
source("./internal/R/load_questionnaire.R")
source("./internal/R/load_analysis_definitions.R")
source("./internal/R/hypegrammar/utilities.R")
source("./internal/R/select_multiplify.R")
# replaced by hypegrammar:
# source("./internal/R/aggregation.R")
# source("./internal/R/KI_aggregation.R")
source("./internal/R/errors.R")
source("./internal/R/questionnaire_skiplogic.R")
source("./internal/R/bonus_visualise_composite.R")

# source("./internal/R/summary_statistics.R")

# stuff for loading/installing packages only when needed:

# checking if package is installed
.is.package.installed<-function(package.name){
  package.name %in% installed.packages()[,"Package"]
}
#  installing dependencies if missing
.install_dependencies<-function(packages.to.install){
  new.packages <- packages.to.install[!.is.package.installed(packages.to.install)]
  if(length(new.packages)) install.packages(new.packages)
  return(packages.to.install)
}

# loading dependencies
.load_dependencies<- function(dependencies){
  sapply(dependencies, require, character.only = TRUE)
  return(dependencies)
}

# load external packages:
.install_dependencies(.dependencies)
.load_dependencies(.dependencies)
# reach "hypegrammar" package ("unpackaged" and loaded as scripts)
source("./internal/R/hypegrammar_dependencies.R")

