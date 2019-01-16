# #### known issues:
# #### brackets that contain no condition by themselves are ignored
# #### e.g. ${a}+2*(3+5)>20 is read as as ${a}+2>20
# question_is_skipped_apply_condition_to_data<-function(data,condition){
#   # print(paste("analysing skiplogic for:",condition))
#   if(condition=="" | is.null(condition)|is.na(condition)){return(rep(FALSE,nrow(data)))}
#   hierarchical_condition_string<-string_w_brackets_to_hierarchical_list(condition)
#   hierarchical_condition_string<-reduce_single_item_lists(hierarchical_condition_string)
#   res<-hierarchical_condition_fulfilled(data,hierarchical_condition_string)
#   res<-remove_junk_from_disected_condition(res)
#   if(is.list(res)){
#     res<-list.clean(res,recursive = T,fun = is.null)
#     res<-reduce_single_item_lists(res)
#   }
#
#   is_skipped<-list_collapse_logic_hierarchy(res)
#   if(!is.vector(is_skipped)){
#     warning("at least parts of skip logic condition `", condition ,"` could not be read properly.")
#     if(is.logical(is_skipped[[1]])){return(is_skipped[[1]])}else{
#     }
#     warning("assuming all records are skipped")
#     return(rep(FALSE,nrow(data)))
#   }
#
#   return(!is_skipped)
# }
#
# # takes a condition that's already split into a hierarchical list and applies it to data
# hierarchical_condition_fulfilled<-function(data,x){
#
#   if(is.list(x)){return(lapply(x,hierarchical_condition_fulfilled,data=data))}else{
#
#     single_condition_fulfilled(data,x)
#   }
# }
#
#
# list_collapse_logic_hierarchy<-function(l){
#   if(!is.list(l)){return(l)}
#
#   # the nested conditions can only be combined if they are in a list of the form:
#   # logical vector - operater string - logical vector - ....
#   # this checks if that is the case:
#
#   # move parts that are only operators up one level:
#   indices_to_flatten_allops<-lapply(l,all_elements_operators) %>% unlist %>% which
#   if(length(indices_to_flatten_allops)>0){
#     l<- flatten_list_items(l,indices_to_flatten_allops)
#   }
#
#
#   # collapse the 'not's (of this level) first:
#   l<-collapse_not_operators(l)
#
#
#   # are there any items of the form: list([logical],[operator])? they must be moved up one level (since they can not be collapsed on their own level..0)
#   # this is an edge case that appears when one side of an operator is nested in extra brackets like this:
#   # ([condition without extra brackets] [and/or] ([condition in extra brackets]))
#   # example:
#   # $ :List of 2
#   # ..$ : logi [1:n] FALSE FALSE FALSE ...
#   # ..$ : chr "|"
#   # $ : logi [1:n] FALSE FALSE FALSE ...
#   indices_to_flatten_endsinoperator<-lapply(l,function(x){
#     collapsable<-is_collapsable_logiclist(x)
#     ends_in_operator<-all_elements_operators(last(x))
#       # return(!collapsable & ends_in_operator)
#     }) %>% unlist %>% which
#
#   if(length(indices_to_flatten_endsinoperator)>0){
#     l<- flatten_list_items(l,indices_to_flatten_endsinoperator)
#   }
#   # collapse the 'not's (of this level) again:
#   l<-collapse_not_operators(l)
#
#
#   # if it's not a logical list combo that I can collapse directly, try collapsing first all sub elements ( - using this fun, so it'srecursive):
#   if(!is_collapsable_logiclist(l)){
#
#     l<-lapply(l,list_collapse_logic_hierarchy)
#     }
#
#
#   # if it's still a list but fully collapsed, return the logical vector:
#   if(is.list(l) & (length(l)==1) & is.logical(l[[1]])){
#     return(l[[1]])
#   }
#   # if I it's still not a collapsable thing.. give up
#   if(!is_collapsable_logiclist(l)){
#     warning("collapsable hierarchy list has a structure that I can't handle:")
#     return(l)
#
#   }
#   # otherwise sweet, let's collapse the first three elements
#   if(l[[2]]=="&"){combined<-l[[1]] & l[[3]]}
#   if(l[[2]]=="|"){combined<-l[[1]] | l[[3]]}
#   l[[1]]<-NULL
#   l[[1]]<-NULL # those two lines do different things, as the "next" first element is deleted on the second row
#   l[[1]]<-combined
#   if(length(l)>1){
#     # there might be more conditions after the first elements that I just collapsed.. if that's the case do those first and then return:
#     return(list_collapse_logic_hierarchy(l))
#   }else{
#     # otherwise all done!
#     return(combined)
# }
# }
#
# is_collapsable_logiclist<-function(l){
#   if(length(l)<3){
#     # if list is less than 3 elements, defo not:
#     goodlisttooperateon<-FALSE
#   }
#   else{
#     # otherwise depends.. is the first and third element a logical vector - and the second element a single element and an operator as a character?
#     goodlisttooperateon<- is.logical(l[[1]]) & is.logical(l[[3]]) & (length(l[[2]])==1) &((l[[2]] %in% c("|","&")) )
#   }
#   goodlisttooperateon
# }
#
#
#
#
# is_a_not_operator<-function(l){
#   if(is.null(l)){return(F)}
#   if(any(is.list(l)) | any(is.na(l)) | is.null(l)){return(F)}
#   if(length(l)!=1){return(F)}
#   return(l=="!")
# }
#
# find_not_operators<-function(l){
#   which(lapply(l,is_a_not_operator) %>% unlist)
# }
#
#
#
#
# collapse_not_operators<-function(l){
# # which list items are 'not' operators?
# not_index<-find_not_operators(l)
# # sort decreasing before looping over so we can remove elements without messing up the indices:
# not_index<-sort(not_index,decreasing = T)
# # for each of them..
# for(i in not_index){
#   # if it's not the last element..
#   if(!(i>=length(l))){
#     # and if the next element is a logical vector..
#     if(is.logical(l[[i+1]])){
#       # apply 'not' to the logical vector
#       l[[i+1]]<- !l[[i+1]]
#       # delete the 'not' operator
#       l[[i]]<-NULL
#
#     }
#   }
#
# }
#
#   return(l)
# }
#
#
# #helper functions for cleaning up nested lists.
#
# # recursively removes all list items that are not logical vectors or
# # strings naming operators (that define how the logical vectors should be combined)
# remove_junk_from_disected_condition<-function(hierarchical_condition){
#   is.junk<-function(x){
#     if(all(is.list(x)) | all(is.logical(x)) | all(x=="|") | all(x=="&") | all(x=="!")){return(FALSE)}
#     return(TRUE)
#   }
#   if(is.list(hierarchical_condition)){
#     if(length(hierarchical_condition)==1){return(remove_junk_from_disected_condition(hierarchical_condition[[1]]))}
#     else{return(lapply(hierarchical_condition,remove_junk_from_disected_condition))}
#
#   }else{
#     if(is.junk(hierarchical_condition)){return(NULL)}
#     return(hierarchical_condition)
#   }
# }
#
# # flattens a list that only has a single item.
# # e.g. turns list(list("A")) into list("A")
# # does that recursively through all subelements of a nested list
# # has unit tests
# reduce_single_item_lists<-function(l){
#   # if l is a single item list, reduce until it no longer is:
#   while(is.list(l) & length(l)==1){l<-l[[1]]}
#   # then do that to all it's elements (recursively:)
#   if(is.list(l)){
#     l<-lapply(l,reduce_single_item_lists)
#   }
#
#   return(l)
#
# }
#
# # BRACKET HIERARCHY LOGIC
# # has unit tests
# string_w_brackets_to_hierarchical_list<-function(x){
#   x_split<-split_on_highest_brackets(x)
#   if(length(x_split)==1){return(x_split %>% split_on_logical_operators)}else{
#     return(lapply(x_split,string_w_brackets_to_hierarchical_list))
#   }
# }
#
# # split a string into a list on the top level bracket
# # has unit tests
# split_on_highest_brackets<-function(x){
#   chars<-lettervector(x)
#   openings<-(chars=="(") %>% as.numeric
#   closings<-(chars==")") %>% as.numeric*-1
#   bracketlevel<-cumsum(openings)+cumsum(c(0,closings[1:(length(closings)-1)]))
#   bracketlevel<-bracketlevel-min(bracketlevel)
#   highest_level<-bracketlevel==0
#   bracketlevel_change<-as.numeric(openings!=0|closings!=0)
#
#   highest_closing_split_positions<-((!highest_level&c(highest_level[-1],FALSE)) %>% which)+1
#   highest_opening_split_positions<-(!highest_level&c(FALSE,highest_level[1:(length(highest_level)-1)])) %>% which
# splitStringAt(x,c(highest_opening_split_positions,highest_closing_split_positions))
# }
#
#
# # INDIVIDUAL CONDITIONS FULFILLED IN DATA? (identify numeric or categorical):
# # returns a logical vector  (condition fulfilled or not in provided records):
#
# # agnostic to condition type:
# single_condition_fulfilled<-function(data,x){
#   if(is_numeric_condition(x)){return(numeric_condition_fulfilled(data,x))}
#   if(is_select_multiple_condition(x)){return(select_multiple_condition_fulfilled(data,x))}
#   if(is_select_one_condition(x)){return(select_one_condition_fulfilled(data,x))}
#   operator_identified<-identify_operator_in_string(x)
#   if(!is.na(operator_identified)){return(operator_identified)}
#   return(x)
# }
#
# # is condition type "selected"?
#
# # is condition type "numric"?
# is_numeric_condition<-function(condition){
#   (strsplit(condition,">|<|>=|<=")[[1]] %>% length)==2
# }
#
# is_select_one_condition<-function(condition){
#   (length(grep("}[[:space:]]*=[[:space:]]*['\"]",condition,fixed = F))!=0)
# }
#
# is_select_multiple_condition<-function(condition){
#   (length(grep("}[[:space:]]*,[[:space:]]*['\"]",condition,fixed = F))!=0)
# }
#
#
#
#
#
# # for selected type: returns a logical vector (condition fulfilled or not in provided records):
# select_multiple_condition_fulfilled<-function(data,condition){
#
#   conditional_var<-strsplit(condition,"\\$\\{")[[1]][2]
#   conditional_var<-  strsplit(conditional_var,"\\}\\,")[[1]][1]
#   conditional_value<-strsplit(condition,'\\}[[:space:]]*,[[:space:]]*["]*')[[1]][2]
#   conditional_value<-strsplit(conditional_value,"\"*)")[[1]][1]
#
#   varname<-to_alphanumeric_lowercase(conditional_var)
#   # if the variable is not in the data we're giving up:
#   if(!(varname %in% names(data))){
#     warning(paste("couldn't figure out part of skip logic condition:\n",condition,"\n", varname," is not a column name in the provided data.\nassuming no records are skipped."))
#     return(rep(FALSE,nrow(data)))}
#
#   lapply(data[,varname],function(x){
#     (conditional_value%in%(x %>% as.character %>% strsplit(" ") %>% unlist))
#   }
#   ) %>% unlist
# }
#
#
#
# mark_operator<-function(x,operator,operator_search=NULL){
#   if(is.null(operator_search)){operator_search<-operator}
#   gsub(paste0("[\\",operator_search,"]"),paste0("HERESANOPERATOR",operator,"HERESANOPERATOR"),x)
#   }
#
#
# kobo_expression_to_valid_r_code<-function(expression){
#   condition<-"${total_children}+${total_children}+1>0+1*5*${total_childrens}"
#   expression<-extract_varname_from_condition(expression)
#   to_compare<-condition
#   to_compare_components<-to_compare %>%
#     mark_operator("\\+") %>%
#     mark_operator("-") %>%
#     mark_operator("/") %>%
#     mark_operator("\\*")  %>%
#     mark_operator(">[^=]") %>%
#     mark_operator(">=") %>%
#     mark_operator("<=")
#   to_compare_components<- strsplit(to_compare_components,"HERESANOPERATOR")[[1]]
#
#
# }
# # for numeric type: returns a logical vector (condition fulfilled or not in provided records):
# numeric_condition_fulfilled<-function(data,condition){
#   # this might be simplified like this:
#   # JUST REMOVE $, { and }, then evaluate and pray!
#   # condition_split<-strsplit(condition, ">|<|>=|<=") %>% unlist
#   # operator_list<-c(">","<","=>","<=")
#   # OPERATOR    <- operator_list[lapply(operator_list,grepl,condition) %>% unlist]
#   # CRITICAL_VALUE<-condition_split[[2]] %>% gsub("\\(|\\)","",.)
#
#   # to_compare<-condition_split[[1]]
#   # # split on mathematical operators +, -, *, /:
#   # to_compare_components<-to_compare %>%
#   #   mark_operator("\\+") %>%
#   #   mark_operator("-") %>%
#   #   mark_operator("/") %>%
#   #   mark_operator("*")  #%>%
#   #   # mark_operator(">") %>%
#   #   # mark_operator(">") %>%
#   #   # mark_operator(">=") %>%
#   #   # mark_operator("<=")
#   # to_compare_components<- strsplit(to_compare_components,"HERESANOPERATOR")[[1]]
#   # # get real varnames:
#   # varnames<-lapply(to_compare_components,extract_varname_from_condition) %>% unlist %>% to_alphanumeric_lowercase
#   # is_variable_name<-!is.na(varnames)
#   # to_compare_components_as_varnames<-to_compare_components
#   # to_compare_components_as_varnames[is_variable_name]<-varnames[is_variable_name]
#
#   # if(any(!(varnames[is_variable_name] %in% names(data)))){
#   #   warning(paste("couldn't figure out skip logic. The following variable names are not a column in the provided data:\n",
#   #                 paste(pastevarnames[!(varnames[is_variable_name] %in% names(data))],collapse="\n")))
#   #   return(rep(TRUE,nrow(data)))}
#   # varnames_subset_rexpression<-to_compare_components_as_varnames
#   # varnames_subset_rexpression[is_variable_name]<-to_compare_components_as_varnames[is_variable_name] %>% lapply(function(x){if(!is.na(x)){return(paste0('data[,"',x,'"]'))}else{return(x)}}) %>% unlist
#   # varnames_subset_rexpression[is.na(varnames_subset_rexpression)]<-to_compare_components[is.na(varnames_subset_rexpression)]
#   # to_compare_rexpression<-varnames_subset_rexpression %>%  paste0(collapse="")
#   # full_expression<- paste0(to_compare_rexpression,OPERATOR,CRITICAL_VALUE)
#   attach(data)
#   full_expression<-rify_varnames_in_string(condition)
#   is_skipped<-eval(parse(text = full_expression))
#   detach(data)
#   return(is_skipped)
# }
#
# # for select one type: returns a logical vector (condition fulfilled or not in provided records):
# select_one_condition_fulfilled<-function(data,condition){
#
#   conditional_var<-strsplit(condition,"\\$\\{")[[1]][2]
#   conditional_var<-  strsplit(conditional_var,"\\}[[:space:]]*\\=[\"']")[[1]][1]
#   conditional_value<-strsplit(condition,"\\}[[:space:]]*\\=[\"']")[[1]][2]
#   conditional_value<-strsplit(conditional_value,"[\"']")[[1]][1]
#
#   varname<-to_alphanumeric_lowercase(conditional_var)
#   # if the variable is not in the data we're giving up:
#   if(!(varname %in% names(data))){
#     warning(paste("couldn't figure out part of skip logic condition:\n",condition,"\n", varname," is not a column name in the provided data.\nassuming no records are skipped."))
#     return(rep(FALSE,nrow(data)))}
#
#     # this is lapplying over rows so super inefficient. Should avoid strsplit, and regex the conditional_value in all rows (considering potential select_multiple!)
#     lapply(data[,varname],function(x){
#     (conditional_value%in%(x %>% as.character %>% strsplit(" ") %>% unlist))
#   }
#   ) %>% unlist
# }
#
#
#
#
# # STRING HELPER FUNCTIONS
#
#
# # concatenate all elements of any number of vectors of strings:
# concat<-function(...){
#   do.call(paste0,list(collapse="",...))
# }
# # split string at a specified character position:
# splitStringAt <- function(x, pos) {
#   x<-lettervector(x)
#   unname(split(x, cumsum(seq_along(x) %in% pos))) %>% lapply(concat )
# }
#
#
# # turns "abc" into c("a","b","c")
# lettervector<-function(char){
#   char %>% strsplit("") %>% unlist
# }
#
#
# # changes different variations of strings containing "and", "or" into "&", "|" or NA (when no "and"/"or" found)
# identify_operator_in_string<-function(x){
#   allowed_before<-c("[[:space:]]","^")
#   allowed_after<-c("[[:space:]]","$")
#   allowed_between_and<-c("and","or")
#   patterns_and<-expand.grid(allowed_before,"and",allowed_after) %>% apply(1,paste0,collapse="")
#   patterns_or<-expand.grid(allowed_before,"or",allowed_after) %>% apply(1,paste0,collapse="")
#
#   count_and<-sapply(patterns_and,grep,x=x) %>% unlist %>% sum
#   count_or<-sapply(patterns_or,grep,x=x) %>% unlist %>% sum
#   if(count_and==1 & count_or<1){return("&")}
#   if(count_or==1 & count_and<1){return("|")}
#   return(NA)
# }
#
# all_elements_operators<-function(l){
#   if(is.null(l)){return(FALSE)}
#   if(any(is.na(l))){return(FALSE)}
#   if(sapply(l,function(x){length(x)>1}) %>% any){return(FALSE)}
#   if(sapply(l,is.list) %>% any){return(FALSE)}
#   if(sapply(l,function(x){x %in% c("|","&","!")}) %>% all){return(TRUE)}
#   return(FALSE)
# }
#
# extract_varname_from_condition<-function(condition){
#   varname<-strsplit(condition,"\\$\\{")[[1]][2]
#   varname<-  strsplit(varname,"\\}")[[1]][1]
#   return(to_alphanumeric_lowercase(varname))
# }
#
# rify_varnames_in_string<-function(x){
#   x<-gsub("[\\{\\$\\}\\(\\)]","",x)
#
# }
#
#
#
#
# split_on_logical_operators<-function(condition){
#
#   split_and_replace_words<-function(x,pattern,replace_pattern_by){
#     x<-paste(" ",x," ")
#     replaced <- x %>% strsplit(pattern) %>% unlist %>% rbind(replace_pattern_by) %>% .[-length(.)] %>% c
#     replaced<-replaced[!(grepl("^[[:space:]]*$",replaced))] # remove empty strings
#     # remove nay amount (*) of spaces at beginnig (^) and end ($) of strings:
#     replaced<- gsub("[[:space:]]*$","",replaced) %>% gsub("^[[:space:]]*","",.)
#   }
#   conditions_split_by_or<-split_and_replace_words(condition,"\\bor\\b","|")
#   conditions_split_by_orand<-sapply(conditions_split_by_or, split_and_replace_words,"\\band\\b","&") %>% unlist %>% unname
#   conditions_split_by_orandnot<-sapply(conditions_split_by_orand,
#                                        split_and_replace_words,
#                                        "[[:space:]]not\\(*[[:space:]]","!") %>% unlist %>% unname
#
#   # turn into list
#   condition_split_all_list <- conditions_split_by_orandnot %>% lapply(function(x){x})
# }
#
