debug(question_is_skipped_apply_condition_to_data)

skipped_auto<-question_is_skipped(data,"yes_no_returnee")
relevant_manual<-(data[,"yes_no_host"]=="no") & (data[,"yes_no_idp"]=="no")
all((skipped_auto==!relevant_manual)) %>% table



skipped_auto<-question_is_skipped(data,"yes_no_returnee")
relevant_manual<-(data[,"yes_no_host"]=="no") & (data[,"yes_no_idp"]=="no")
all((skipped_auto==!relevant_manual)) %>% table

any(!skipped_auto & data$yes_no_host!="no")
any(!skipped_auto & data$dest_loc_why1=="none")
undebug(list_collapse_logic_hierarchy)
debug(string_w_brackets_to_hierarchical_list)
skipped_auto<-question_is_skipped(data,"dest_loc_why2")
# ( selected(${yes_no_host},""no"") ) and ( not(selected(${dest_loc_why1},""none"")) and not(selected(${dest_loc_why1},""dontknow"")) )
relevant_manual<-(data$yes_no_host=="no") & (!(grepl("none",data[,"dest_loc_why1"])) & !(grepl("dontknow",data[,"dest_loc_why1"])))
(list(skipped_auto,relevant_manual)) %>% table

data[,"dest_loc_why1"]



condition<-"selected(${yes_no_host},\"\"no\"\") ) and ( not(selected(${dest_loc_why1},\"\"none\"\")) and not(selected(${dest_loc_why1},\"\"dontknow\"\"))"

string_w_brackets_to_hierarchical_list(cond) %>% reduce_single_item_lists %>% glimpse

undebug(string_w_brackets_to_hierarchical_list)
undebug(split_on_highest_brackets)
debug(list_collapse_logic_hierarchy)
hierarchical_condition_string<-string_w_brackets_to_hierarchical_list(condition)
hierarchical_condition_string<-reduce_single_item_lists(hierarchical_condition_string)
hierarchical_condition_string %>% glimpse
res<-hierarchical_condition_fulfilled(data,hierarchical_condition_string)
res<-remove_junk_from_disected_condition(res)
res<-list.clean(res,recursive = T,fun = is.null)
res<-reduce_single_item_lists(res)
res %>% glimpse
debug(list_collapse_logic_hierarchy)
list_collapse_logic_hierarchy(res) %>% glimpse

