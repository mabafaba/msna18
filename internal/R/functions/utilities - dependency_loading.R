# sourcing all scripts in a directory
.source_dir<-function(path,...){
  files<-list.files(path)
  sourcefiles<-paste0(path,"/",files)
  sourcefiles<-gsub("\\/\\/","\\/",sourcefiles)
  sourcefiles<-grep("\\.R$",sourcefiles,value=T)
  lapply(sourcefiles,source,...)
}


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
