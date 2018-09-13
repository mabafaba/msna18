# setwd to current

if(!("rstudioapi" %in% installed.packages()[,"Package"])){install.packages("rstudioapi")};require("rstudioapi");setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())

# load all the codes, libraries etc.:

getwd()
source("./internal/R/dependencies.R")
list.files()
data<-read.csv("./internal/input_files/data.csv",stringsAsFactors = F)
# load questionnaire and create associated functions:

### Make sure that your version of questionnaire substitutes _ for . in both the names of 
### the questions, names of the data and the choices
questionnaire<-load_questionnaire(data = data, 
                                  questions.file = "./internal/input_files/kobo questions.csv",
                                  choices.file = "./internal/input_files/kobo choices.csv",
                                  choices.label.column.to.use = "name")
### Also - use the updated dataset: called LBY_names_fixed


# take last it of splitted string
sp_multiple<-function(x,sep){tail(strsplit(x,sep)[[1]],1)}


concatenate_sm_var<-function(questionnaire,my_sm_vars){
  
  # my_sm_vars: the select_multiple variable from the questionnaire
  pattern=paste0(my_sm_vars,"\\.")
  
  #get the var names
  sm_name<-names(questionnaire)[grep(pattern,names(questionnaire))]

  # extract the choices names from headings
  ch<-lapply(sm_name,sp_multiple,sep=pattern) %>% unlist
  
  # get the choices from questionnaire
  choices_questionnaire<-questionnaire$choices_per_variable[[my_sm_vars]]$name
  
  
  # check if it matchs with questionnaires
  if(!all(ch %in% choices_questionnaire)){
    # it does not but maybe we fixe it with make.names
    if(all(ch %in% make.names(choices_questionnaire))){
      # reorder questionnaire choices to follow data order
      ch<-choices_questionnaire[match(ch,make.names(choices_questionnaire))]
    } else {
      print(paste0(my_sm_vars,": the headings choices do not match the questionnaire choices"))
    }
  }
  
  # here assumption that is 0/1 or TRUE/FALSE only - no characters
  noname<-function(vec,ch){
    ch[vec%in%c(1,TRUE)] %>% paste(collapse=" ")
  }
  
  # extract the data
  sm_data<-questionnaire[sm_name] %>% as.data.frame

  # contecatenate the choices
  var_sm_out<-apply(sm_data,1,noname,ch=ch)
  
  # would return the data
  # sm_data[[my_sm_vars]]<-var_sm_out
  # return(sm_data)
  
  # just return the vector
  return(var_sm_out)
}
  
# get smultiple col names
sm_variables<-names(questionnaire)[lapply(names(questionnaire),question_is_select_multiple) %>% unlist] 
sm_rows <- lapply(sm_variables, function(x){grep(x,names(data))}) %>% unlist

# apply the function
myset<-lapply(sm_variables,concatenate_sm_var,questionnaire=questionnaire) %>% do.call(cbind, .)

colnames(myset) = sm_variables

#### FIX IT FOR THE OTHERS 
data_no_sm2 <- data[,-(sm_rows)] 
data_no_sm2 <- data_no_sm2[-c(1, 2,3,4,5)] ## dont know why but the gps part made the code crashed so I have just removed it
names(data_no_sm2) <- gsub("_", ".", names(data_no_sm2))


convert_to_xml_all_others <- function(data_no_sm, questionnaire, choices.column.to.use){

  no_sm_vars <- names(data_no_sm) %>% to_alphanumeric_lowercase
  names(data_no_sm) %<>% to_alphanumeric_lowercase

recoded_data <- lapply(no_sm_vars, function(var){
    
    choices_questionnaire <- questionnaire$choices_per_variable[[var]]$name
    ch <- unique(data_no_sm[[var]])
    if(question_is_select_one(var) & !all(ch %in% choices_questionnaire)){
      # it does not but maybe we fixe it with make.names
      # if(!all(ch %in% make.names(choices_questionnaire))){
        # reorder questionnaire choices to follow data order
      varasnames<-data_no_sm[[var]]
        for(i in ch){
          if(!(i %in% (choices_questionnaire))){
            i <- trimws(i, which = "right")
            to <- match(i, trimws(questionnaire$choices_per_variable[[var]]$label, which = "right")) %>% unlist
            varasnames[data_no_sm[[var]] == i] <- questionnaire$choices_per_variable[[var]]$name[to] 
              # replace(data_no_sm[[var]], (data_no_sm[[var]] == i), questionnaire$choices_per_variable[[var]]$name[to])
            }else{print(paste0(var,": the headings choices do not match the questionnaire choices"))}
        }
      varasnames[is.na(varasnames)] <- data_no_sm[is.na(varasnames),var]
      
      # }
      return(varasnames)
    }else{
      return(data_no_sm[,var])
    }
    
    
    }) # %>% do.call(data.frame,.)
return(recoded_data)
}

named<-convert_to_xml_all_others(data_no_sm2, questionnaire, choices.column.to.use = NA) 
named %>% tail


##puting everything together back
named <- as.data.frame(named)
names(named) <-names(data_no_sm2)
myset <- as.data.frame(myset)
names(myset) <- sm_variables
first_part <- data[,c(1:5)]
choices_for_select <- data[,(sm_rows)]
names(choices_for_select) %<>% gsub("_", ".", .)
choices_for_select <- choices_for_select[,-(which(names(choices_for_select) %in% sm_variables))]
weights<- data[,] ## to get the few columns removed because of issues

alltogether <- cbind(first_part, named,myset, choices_for_select)
names(alltogether)

write.csv(alltogether, "dataxml.csv")




