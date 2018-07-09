install.packages("rlist")
require("rlist")
require("dplyr")
condition<-'(${calc_boys_ed} + ${calc_girls_ed} > 0) and (selected(${head_of_household}, "yes") or (selected(${hoh_equivalent}, "yes") or BETWEEN REGULAR ORS or SOMETHING or (selected(${hoh_equivalent}, "maybe")))) and (LAST)'
data<-data.frame(calc.boys.ed=runif(100),calc.girls.ed=-runif(100),head.of.household= sample(c("yes","no","maybe"),100,T),hoh.equivalent= sample(c("yes","no","maybe"),100,T))
hierarch<-string_w_brackets_to_hierarchical_list(condition)
res<-hierarchical_condition_fulfilled(data,hierarch)
res<-remove_junk_from_disected_condition(res)
res<-list.clean(res,recursive = T,fun = is.null)
res<-list.clean(res,recursive = T,fun = function(x){length(x)==0})
list.apply(res,length,recursive=T)

remove_junk_from_disected_condition<-function(hierarchical_condition){
   is.junk<-function(x){
     if(is.list(x) | is.logical(x) | x=="|" | x=="&"){return(FALSE)}
     return(TRUE)
   }
if(is.list(hierarchical_condition)){
  if(length(hierarchical_condition)==1){return(remove_junk_from_disected_condition(hierarchical_condition[[1]]))}
  else{return(lapply(hierarchical_condition,remove_junk_from_disected_condition))}

  }else{
  if(is.junk(hierarchical_condition)){return(NULL)}
  return(hierarchical_condition)
}
   
}


reduce_single_item_lists<-function(l){
  if(!is.list(l)){return(l)}
  if(length(l)==1){l<-l[[1]]}
  if(!is.list(l)){return(l)}
  lapply(l,reduce_single_item_lists)
  }




undebug(reduce_single_item_lists)
l<-list(list(1,list(1)),list(1))
reduce_single_item_lists(l)

res<-reduce_single_item_lists(res)
l<-res[[3]][[3]]
res[[3]] %>% glimpse

list_operate_logic<-function(l){
  if(!is.list(l)){return(l)}
goodlisttooperateon<- is.logical(l[[1]]) & is.logical(l[[3]]) & (l[[2]] %in% c("|","&") )
if(!goodlisttooperateon){lapply(l,list_operate_logic)}

if(l[[2]]=="&"){combined<-l[[1]] | l[[3]]}
if(l[[2]]=="|"){combined<-l[[1]] & l[[3]]}
  l[[1]]<-NULL
  l[[1]]<-NULL
  l[[1]]<-combined
  if(length(l)>=1){
    return(list_operate_logic(l))
  }else{
    return(combined)
  }
}


debug(list_operate_logic)
list_operate_logic(res)




is_selected_condition<-function(condition){
  (length(grep('}, "',condition,fixed = T))!=0)|(length(grep("}, '",condition,fixed = T))!=0)
}


is_numeric_condition<-function(condition){
  (strsplit(condition,">|<|>=|<=")[[1]] %>% length)==2
}



selected_condition_fulfilled<-function(data,condition){
  
  conditional_var<-strsplit(condition,"\\$\\{")[[1]][2] 
  conditional_var<-  strsplit(conditional_var,"\\}\\,")[[1]][1]
  conditional_value<-strsplit(condition,'}, "')[[1]][2]
  conditional_value<-strsplit(conditional_value,"\")")[[1]][1]
  
  varname<-to_alphanumeric_lowercase(conditional_var)
  
  lapply(data[,varname],function(x){
    (conditional_value%in%(x %>% as.character %>% strsplit(" ") %>% unlist))
  }
  ) %>% unlist
}


extract_varname_from_condition<-function(condition){
  varname<-strsplit(condition,"\\$\\{")[[1]][2] 
  varname<-  strsplit(varname,"\\}")[[1]][1]
  return(to_alphanumeric_lowercase(varname))
}






            






numeric_condition_fulfilled<-function(data,condition){
  condition_split<-strsplit(condition, ">|<|>=|<=") %>% unlist
  operator_list<-c(">","<","=>","<=")
  OPERATOR    <- operator_list[lapply(operator_list,grepl,condition) %>% unlist]
  CRITICAL_VALUE<-condition_split[[2]] %>% gsub("\\(|\\)","",.) %>% as.numeric

  to_compare<-condition_split[[1]]
  to_compare_components<-gsub("\\+","HERESANOPERATOR+HERESANOPERATOR",to_compare)
  to_compare_components<- strsplit(to_compare_components,"HERESANOPERATOR")[[1]]
  to_compare_components_as_varnames<-lapply(to_compare_components,extract_varname_from_condition) %>% unlist %>% to_alphanumeric_lowercase
  # lapply(to_compare_components_as_varnames,function(x){if(!is.na(x)){return(data[,x])}else{NA}})
  varnames_subset_rexpression<-to_compare_components_as_varnames %>% lapply(function(x){if(!is.na(x)){return(paste0('data[,"',x,'"]'))}else{return(x)}}) %>% unlist
  varnames_subset_rexpression[is.na(varnames_subset_rexpression)]<-to_compare_components[is.na(varnames_subset_rexpression)]   
  to_compare_rexpression<-varnames_subset_rexpression %>%  paste0(collapse="")
  full_expression<- paste0(to_compare_rexpression,OPERATOR,CRITICAL_VALUE)
  
  is_skipped<-eval(parse(text = full_expression))
  return(is_skipped)
  }



lettervector<-function(char){
  char %>% strsplit("") %>% unlist
}



hierarch
hierarchical_condition_fulfilled<-function(data,x){
  if(is.list(x)){return(lapply(x,hierarchical_condition_fulfilled,data=data))}else{
    
    single_condition_fulfilled(data,x)
  }
}


single_condition_fulfilled<-function(data,x){
if(is_numeric_condition(x)){return(numeric_condition_fulfilled(data,x))}
if(is_selected_condition(x)){return(selected_condition_fulfilled(data,x))}
operator_identified<-identify_operator_in_string(x)
if(!is.na(operator_identified)){return(operator_identified)}
  return(x)
  }



identify_operator_in_string<-function(x){
  allowed_before<-c("[[:space:]]","^")
  allowed_after<-c("[[:space:]]","$")
  allowed_between_and<-c("and","or")
  patterns_and<-expand.grid(allowed_before,"and",allowed_after) %>% apply(1,paste0,collapse="")
  patterns_or<-expand.grid(allowed_before,"or",allowed_after) %>% apply(1,paste0,collapse="")
  
  count_and<-sapply(patterns_and,grep,x=x) %>% unlist %>% sum 
  count_or<-sapply(patterns_or,grep,x=x) %>% unlist %>% sum
  if(count_and==1 & count_or<1){return("&")}
  if(count_or==1 & count_and<1){return("|")}
  return(NA)
}



concat<-function(...){
  do.call(paste0,list(collapse="",...))
}

splitStringAt <- function(x, pos) {
  x<-lettervector(x)
  unname(split(x, cumsum(seq_along(x) %in% pos))) %>% lapply(concat ) 
  }













string_w_brackets_to_hierarchical_list<-function(x){
  x_split<-split_on_highest_brackets(x)
  if(length(x_split)==1){return(x_split)}else{
    return(lapply(x_split,string_w_brackets_to_hierarchical_list))
  }
}


split_on_highest_brackets<-function(x){
  chars<-lettervector(x)
  openings<-(chars=="(") %>% as.numeric
  closings<-(chars==")") %>% as.numeric*-1
  bracketlevel<-cumsum(openings)+cumsum(c(0,closings[1:(length(closings)-1)]))
  bracketlevel<-bracketlevel-min(bracketlevel)
  highest_level<-bracketlevel==0 
  bracketlevel_change<-as.numeric(openings!=0|closings!=0)
  
  highest_closing_split_positions<-((!highest_level&c(highest_level[-1],FALSE)) %>% which)+1
  highest_opening_split_positions<-(!highest_level&c(FALSE,highest_level[1:(length(highest_level)-1)])) %>% which
splitStringAt(x,c(highest_opening_split_positions,highest_closing_split_positions))  
}


















# (condition %>% strsplit("and \\("))[[1]] %>% lapply(strsplit,split = "or \\(") %>% lapply(unlist) %>% lapply(lapply,function(x){
#   strsplit("and \\("))[[1]] %>% lapply(strsplit,split = "or \\(")
# })
# 
# condition %>% strsplit("\\(") %>% lapply(function(x){
#   x  %>% gsub("and","THERESALOGICALOPERATORandTHERESALOGICALOPERATOR",.)%>% gsub("or","THERESALOGICALOPERATORorTHERESALOGICALOPERATOR",.) %>% strsplit("THERESALOGICALOPERATOR")
# })





# questionnaire_get_kobo_skipcondition(varname){
#   
# }
# 



