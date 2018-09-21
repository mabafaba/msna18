# easier dependency loading:
source("./internal/R/functions/utilities - dependency_loading.R")
# folders to load scripts from
source_folders<-c("./internal/R/hypegrammaR",
                  "./internal/R/koboreadeR",
                  "./internal/R/functions")
# source them all:
sapply(source_folders,.source_dir)
# external packages to install/load:
.dependencies<-c("dplyr","purrr","questionr","data.table","tidyr","crayon","magrittr","rstudioapi","igraph","knitr","rlist","ggplot2","ggthemes","extrafont","gridExtra")



# load external packages:
.install_dependencies(.dependencies)
.load_dependencies(.dependencies)
# reach "hypegrammaR" package ( dependencies installed and loaded, hypegrammaR loaded locally from .R files)
source("./internal/R/121 - hypegrammar_dependencies.R")

