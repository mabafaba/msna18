


question_is_skipped<-function(data,condition){
  print(paste("analyising skiplogic for:",condition))
  if(condition=="" | is.null(condition)|is.na(condition)){return(rep(nrow(data),FALSE))}
  
  hierarchical_condition_string<-string_w_brackets_to_hierarchical_list(condition)
  res<-hierarchical_condition_fulfilled(data,hierarchical_condition_string)
  
  res<-remove_junk_from_disected_condition(res)
  res<-list.clean(res,recursive = T,fun = is.null)
  res<-reduce_single_item_lists(res)
  is_skipped<-list_collapse_logic_hierarchy(res)
  if(!is.vector(is_skipped)){
    warning("at least parts of skip logic condition `", condition ,"` could not be read properly.")
    if(is.logical(is_skipped[[1]])){return(is_skipped[[1]])}
  }
  
  return(is_skipped)
}

# takes a condition that's already split into a hierarchical list and applies it to data
hierarchical_condition_fulfilled<-function(data,x){
  if(is.list(x)){return(lapply(x,hierarchical_condition_fulfilled,data=data))}else{
    
    single_condition_fulfilled(data,x)
  }
}



list_collapse_logic_hierarchy<-function(l){
  if(!is.list(l)){return(l)}
  
  # the nested conditions can only be combined if they are in a list of the form:
  # logical vector - operater string - logical vector - ....
  # this checks if that is the case:
  
  # if it's not a logical list combo that I can collapse directly, try collapsing first all sub elements ( - using this fun, so it'srecursive):
  if(!is_collapsable_logiclist(l)){l<-lapply(l,list_collapse_logic_hierarchy)}
  # if I it's still not a collapsable thing.. give up 
  if(!is_collapsable_logiclist(l)){
    warning("collapsable hierarchy list has a structure that I can't handle:")
    return(l)
    
  }
  # otherwise sweet, let's collapse the first three elements
  if(l[[2]]=="&"){combined<-l[[1]] | l[[3]]}
  if(l[[2]]=="|"){combined<-l[[1]] & l[[3]]}
  l[[1]]<-NULL
  l[[1]]<-NULL
  l[[1]]<-combined
  if(length(l)>1){
    # there might be more conditions after the first elements that I just collapsed.. if that's the case do those first and then return: 
    return(list_collapse_logic_hierarchy(l))
  }else{
    # otherwise all done!
    return(combined)
  }
}


is_collapsable_logiclist<-function(l){
  if(length(l)<3){
    # if list is less than 3 elements, defo not:
    goodlisttooperateon<-FALSE
  }
  else{
    # otherwise depends.. is the first and third element a logical vector - and the second element an operator as a character?
    goodlisttooperateon<- is.logical(l[[1]]) & is.logical(l[[3]]) & (l[[2]] %in% c("|","&") )
  }
  goodlisttooperateon
}





#helper functions for cleaning up nested lists.


# recursively removes all list items that are not logical vectors or 
# strings naming operators (that define how the logical vectors should be combined)
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

# flattens a list that only has a single item.
# e.g. turns list(list("A")) into list("A")
# does that recursively through all subelements of a nested list
reduce_single_item_lists<-function(l){
  # if l is a single item list, reduce until it no longer is:
  while(is.list(l) & length(l)==1){l<-l[[1]]}
  # then do that to all it's elements (recursively:)
  if(is.list(l)){
    l<-lapply(l,reduce_single_item_lists)
  }
  
  return(l)
  
}





# BRACKET HIERARCHY LOGIC
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






# INDIVIDUAL CONDITIONS FULFILLED IN DATA? (identify numeric or categorical):
# returns a logical vector  (condition fulfilled or not in provided records):

# agnostic to condition type:
single_condition_fulfilled<-function(data,x){
  if(is_numeric_condition(x)){return(numeric_condition_fulfilled(data,x))}
  if(is_selected_condition(x)){return(selected_condition_fulfilled(data,x))}
  operator_identified<-identify_operator_in_string(x)
  if(!is.na(operator_identified)){return(operator_identified)}
  return(x)
}

# is condition type "selected"?
is_selected_condition<-function(condition){
  (length(grep('}, "',condition,fixed = T))!=0)|(length(grep("}, '",condition,fixed = T))!=0)
}
# is condition type "numric"?
is_numeric_condition<-function(condition){
  (strsplit(condition,">|<|>=|<=")[[1]] %>% length)==2
}





# for selected type: returns a logical vector (condition fulfilled or not in provided records):
selected_condition_fulfilled<-function(data,condition){
  
  conditional_var<-strsplit(condition,"\\$\\{")[[1]][2] 
  conditional_var<-  strsplit(conditional_var,"\\}\\,")[[1]][1]
  conditional_value<-strsplit(condition,'}, "')[[1]][2]
  conditional_value<-strsplit(conditional_value,"\")")[[1]][1]
  
  varname<-to_alphanumeric_lowercase(conditional_var)
  # if the variable is not in the data we're giving up:
  if(!(varname %in% names(data))){
    warning(paste("couldn't figure out skip logic:",varname,"is not a column name in the provided data."))
    return(rep(TRUE,nrow(data)))}
  
  lapply(data[,varname],function(x){
    (conditional_value%in%(x %>% as.character %>% strsplit(" ") %>% unlist))
  }
  ) %>% unlist
}



# for numeric type: returns a logical vector (condition fulfilled or not in provided records):

numeric_condition_fulfilled<-function(data,condition){
  condition_split<-strsplit(condition, ">|<|>=|<=") %>% unlist
  operator_list<-c(">","<","=>","<=")
  OPERATOR    <- operator_list[lapply(operator_list,grepl,condition) %>% unlist]
  CRITICAL_VALUE<-condition_split[[2]] %>% gsub("\\(|\\)","",.) %>% as.numeric
  
  to_compare<-condition_split[[1]]
  to_compare_components<-gsub("\\+","HERESANOPERATOR+HERESANOPERATOR",to_compare)
  to_compare_components<- strsplit(to_compare_components,"HERESANOPERATOR")[[1]]
  to_compare_components_as_varnames<-lapply(to_compare_components,extract_varname_from_condition) %>% unlist %>% to_alphanumeric_lowercase
  if(!(to_compare_components_as_varnames %in% names(data))){
    warning(paste("couldn't figure out skip logic:",varname,"is not a column name in the provided data."))
    return(rep(TRUE,nrow(data)))}
  # lapply(to_compare_components_as_varnames,function(x){if(!is.na(x)){return(data[,x])}else{NA}})
  varnames_subset_rexpression<-to_compare_components_as_varnames %>% lapply(function(x){if(!is.na(x)){return(paste0('data[,"',x,'"]'))}else{return(x)}}) %>% unlist
  varnames_subset_rexpression[is.na(varnames_subset_rexpression)]<-to_compare_components[is.na(varnames_subset_rexpression)]   
  to_compare_rexpression<-varnames_subset_rexpression %>%  paste0(collapse="")
  full_expression<- paste0(to_compare_rexpression,OPERATOR,CRITICAL_VALUE)
  is_skipped<-eval(parse(text = full_expression))
  return(is_skipped)
}





# STRING HELPER FUNCTIONS


# concatenate all elements of any number of vectors of strings:
concat<-function(...){
  do.call(paste0,list(collapse="",...))
}
# split string at a specified character position:
splitStringAt <- function(x, pos) {
  x<-lettervector(x)
  unname(split(x, cumsum(seq_along(x) %in% pos))) %>% lapply(concat ) 
}


# turns "abc" into c("a","b","c")
lettervector<-function(char){
  char %>% strsplit("") %>% unlist
}


# changes different variations of strings containing "and", "or" into "&", "|" or NA (when no "and"/"or" found)
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


extract_varname_from_condition<-function(condition){
  varname<-strsplit(condition,"\\$\\{")[[1]][2] 
  varname<-  strsplit(varname,"\\}")[[1]][1]
  return(to_alphanumeric_lowercase(varname))
}






