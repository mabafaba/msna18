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
# # install reachR
# .install_reachR<-function(reinstall_if_exists = F, branch="master"){
#   if(!.is.package.installed("reachR") | reinstall_if_exists){
#     # get devtools if needed
#     if(!.is.package.installed("devtools")){install.packages("devtools")}
#     require("devtools")
#     install_github("mabafaba/reachR",ref = branch, auth_token = "dda123a51bd8fc6c2d0c47208ba6df17abee8b82")
#     # unload devtools
#     detach("package:devtools", unload=TRUE)
#     if(reinstall_if_exists){
#       warning("Please restart R session to update reachR documentation")
#     }
#   }
# }
# load dependencies
.load_dependencies<- function(dependencies){
  sapply(dependencies, require, character.only = TRUE)
  return(dependencies)
}


.dependencies<-c("dplyr","ggplot2","ggthemes","reshape2","questionr", "survey")
.install_dependencies(.dependencies)
# .install_reachR(reinstall_if_exists = F)
# .load_dependencies(c(.dependencies,"reachR"))


dependencies_spatial<-c("rgdal","raster","RgoogleMaps","ggmap","gstat")
dependencies_spatial %>% .install_dependencies %>% .load_dependencies
