# load scripts
hypegrammaR_source_path<-paste("./internal/R/hypegrammaR/")
hypegrammaR_source_files<-list.files(hypegrammaR_source_path) %>% paste0(hypegrammaR_source_path,.)
source_loading_log<-sapply(hypegrammaR_source_files,source,verbose=F)


# check if package is installed
.is.package.installed<-function(package.name){
  package.name %in% installed.packages()[,"Package"]
}
#  install dependencies if missing
.install_dependencies<-function(packages.to.install){
  new.packages <- packages.to.install[!.is.package.installed(packages.to.install)]
  if(length(new.packages)) install.packages(new.packages)
  return(packages.to.install)
}

# load dependencies
.load_dependencies<- function(dependencies){
  sapply(dependencies, require, character.only = TRUE)
  return(dependencies)
}


.dependencies<-c("dplyr","ggplot2","ggthemes","reshape2","questionr", "survey")
.install_dependencies(.dependencies)
.load_dependencies(c(.dependencies))


dependencies_spatial<-c("rgdal","raster","RgoogleMaps","ggmap","gstat")
dependencies_spatial %>% .install_dependencies %>% .load_dependencies
