

condition<-"selected(${respondent_type}, \"\"host_community\"\")"

skipped_condition<-function(condition){
  
            extract_varname<-function(condition){
            
            }
            

is_selected_condition<-function(condition){
  length(grep("^selected\\(\\$\\{",condition))!=0
}            

selected_condition<-function(data,condition){
  
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


condition<-'(${calc_boys_ed} + ${calc_girls_ed} > 0)'
data<-data.frame(calc.boys.ed=runif(100),calc.girls.ed=runif(100))

numeric_condition<-function(data,condition){
  condition_split<-strsplit(condition, ">|<|>=|<=") %>% unlist
  
  operator_list<-c(">","<","=>","<=")
  operator    <- operator_list[lapply(operator_list,grepl,condition) %>% unlist]
  to_compare<-condition_split[[1]]
  critical_value<-condition_split[[2]] %>% gsub("\\(|\\)","",.) %>% as.numeric
  # to_compare_components<-strsplit(to_compare,"\\$\\{")[[1]][-1]
  
  to_compare_components<-gsub("\\+","HERESANOPERATOR+HERESANOPERATOR",to_compare)
  to_compare_components<- strsplit(to_compare_components,"HERESANOPERATOR")[[1]]
  to_compare_components_as_varnames<-lapply(to_compare_components,extract_varname_from_condition) %>% unlist %>% to_alphanumeric_lowercase
  # lapply(to_compare_components_as_varnames,function(x){if(!is.na(x)){return(data[,x])}else{NA}})
  varnames_subset_rexpression<-to_compare_components_as_varnames %>% lapply(function(x){if(!is.na(x)){return(paste0('data[,"',x,'"]'))}else{return(x)}}) %>% unlist
  varnames_subset_rexpression[is.na(varnames_subset_rexpression)]<-to_compare_components[is.na(varnames_subset_rexpression)]   
  to_compare_rexpression<-varnames_subset_rexpression %>%  paste0(collapse="")
  to_compare_rexpression(paste0(to_compare_rexpression,"",""))
  eval(parse(text = to_compare_rexpression))

  extract_varname_from_condition("+") %>% print
}


extract_varname_from_condition<-function(condition){
  varname<-strsplit(condition,"\\$\\{")[[1]][2] 
  varname<-  strsplit(conditional_var,"\\}")[[1]][1]
  return(to_alphanumeric_lowercase(varname))
}

condition<-"a<1"
is_numeric_condition(condition)






openings<-((condition %>% strsplit(""))[[1]]=="(") %>% as.numeric
closings<-((condition %>% strsplit(""))[[1]]==")") %>% as.numeric
bracketlevel<-cumsum(openings-closings)

(condition %>% strsplit(""))[[1]][bracketlevel==3] %>% paste0(collapse=""))


condition[bracketlevel==3]

condition<-'(${calc_boys_ed} + ${calc_girls_ed} > 0) and (selected(${head_of_household}, "yes") or selected(${hoh_equivalent}, "yes"))'


condition<-"FIRST1 and ( SECOND1 and (THIRD1 or (FOURTH1 or FOURTH2))) "

(condition %>% strsplit("and \\("))[[1]] %>% lapply(strsplit,split = "or \\(") %>% lapply(unlist) %>% lapply(lapply,function(x){
  strsplit("and \\("))[[1]] %>% lapply(strsplit,split = "or \\(")
})

condition %>% strsplit("\\(") %>% lapply(function(x){
  x  %>% gsub("and","THERESALOGICALOPERATORandTHERESALOGICALOPERATOR",.)%>% gsub("or","THERESALOGICALOPERATORorTHERESALOGICALOPERATOR",.) %>% strsplit("THERESALOGICALOPERATOR")
})
condition 



is_numeric_condition("a>=2")


is_variable_condition_fulfilled<-function(x)





# questionnaire_get_kobo_skipcondition(varname){
#   
# }
# 



