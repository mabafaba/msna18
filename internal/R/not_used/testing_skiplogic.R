# 
# skipped_auto<-question_is_skipped(data,"yes_no_returnee")
# relevant_manual<-(data[,"yes_no_host"]=="no") & (data[,"yes_no_idp"]=="no")
# all((skipped_auto==!relevant_manual)) %>% table
# 
# skipped_auto<-question_is_skipped(data,"yes_no_returnee")
# relevant_manual<-(data[,"yes_no_host"]=="no") & (data[,"yes_no_idp"]=="no")
# all((skipped_auto==!relevant_manual)) %>% table
# 
# skipped_auto<-question_is_skipped(data,"dest_loc_why2")
# # ( selected(${yes_no_host},""no"") ) and ( not(selected(${dest_loc_why1},""none"")) and not(selected(${dest_loc_why1},""dontknow"")) )
# relevant_manual<-(data$yes_no_host=="no") & (!(grepl("none",data[,"dest_loc_why1"])) & !(grepl("dontknow",data[,"dest_loc_why1"])))
# all((skipped_auto==!relevant_manual))
# 
