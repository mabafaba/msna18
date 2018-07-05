

condition<-"selected(${respondent_type}, \"\"host_community\"\")"

skipped_condition<-function(condition){
            extract_varname<-function(condition){
            }
            

is_selected_condition<-function(condition){
  length(grep("^selected\\(\\$\\{",condition))!=0
}            

selected_condition<-function(data,varname,condition){
  
  conditional_var<-strsplit(condition,"^selected\\(\\$\\{")[[1]][2] 
  conditional_var<-  strsplit(conditional_var,"\\}\\,")[[1]][1]
  conditional_value<-strsplit(condition,"\\}\\,\\ \"\"")[[1]][2]
  conditional_value<-strsplit(conditional_value,"\"\"\\)")[[1]][1]
  
  varname<-to_alphanumeric_lowercase(conditional_var)
  
    lapply(data[,conditional_var],function(x){
      (conditional_value%in%(x %>% strsplit("") %>% unlist))
      }
      ) %>% unlist
  }
 
is_numeric_condition<-function(condition){
  (strsplit(condition,">|<|>=|<=")[[1]] %>% length)==2
}

numeric_condition<-function(condition){
  grepl(">")
  strsplit(condition ">|<|>=|<=")
}


condition<-"a<1"
is_numeric_condition(condition)


is_numeric_condition("a>=2")


is_variable_condition_fulfilled<-function(x)





# questionnaire_get_kobo_skipcondition(varname){
#   
# }
# 



