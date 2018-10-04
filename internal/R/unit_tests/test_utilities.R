# where are the examples stored?

example.data.path<-function(examplename){
  return(paste0("./test_data/",examplename,"/"))
}


# set available examples' metadata:

example_names<-c("example1","example2")
# Metadata for examples.
# Data frame with one row per

example_metadata<-data.frame(
  name=example_names,
  path=sapply(example_names,example.data.path) %>% unname,
  choice.label.column.to.use=c("Label::English","Label::English"),
  stringsAsFactors = F
)

# two ways to load examples:
## example <- load.example("example1")
  ## names(example$data)
## load_example("example1,global_space=T)
  ## names(data)
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################



read.example.csv<-function(filename,examplename){
  read.csv.auto.sep(paste0(example.data.path(examplename),filename))
}



load.example<-function(name,global_space=F){

  
  ex<-example_metadata[which(example_names==name),,drop=F] %>% as.list
  
  exfile<-function(file){
    read.example.csv(file,ex$name)
  }
  exfilepath<-function(file){
    paste0(ex$path,file)
  }
  
  ex$data<-exfile("data.csv")
  ex$questionnaire<-load_questionnaire(ex$data,
                                        questions.file = exfilepath("kobo questions.csv"),
                                        choices.file = exfilepath("kobo choices.csv"),
                                        choices.label.column.to.use = ex$choice.label.column.to.use)
  if(global_space){
    data<<-ex$data
    questionnaire<<-ex$questionnaire
    return(NULL)
  }
  
  return(ex)
}  






