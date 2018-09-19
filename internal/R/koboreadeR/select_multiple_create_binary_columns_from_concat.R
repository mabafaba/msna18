
expand.select.multiple.to.binarydf<-function(df){

  lapply(names(df),function(varname){

allchoices<-df[[varname]] %>% as.character %>% strsplit(" ") %>% unlist %>% unique
choicesvec<-df[[varname]] %>% as.character %>% strsplit(" ")
logicaldf<-lapply(allchoices,function(x){
  lapply(choicesvec,function(thesechoices){
    if(is.na(thesechoices[1])){return(NA)}
    x %in% thesechoices
    
    }) %>% unlist
})

names(logicaldf)<-paste(varname,allchoices,sep=".")
logicaldf %>% as.data.frame

}) %>% do.call(cbind,.)
  
}
