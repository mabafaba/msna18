

htmlreport<-function(results){
source("./internal/R/htmlR.R")

dir.create("./output/summary")
.record(to.file = "output/summary/index.html")
.rewind()
.scream("Overview")
.say(paste("Number of records:",.saybold(nrow(data))))
.saytag("br")
.say(paste("NA (including skiplogic):",.saybold((length(which(is.na(as.vector(data))))/length(do.call(c,data))*100) %>% round),"%"))
.scream("Composite indicators")
.shout("Structure")
.saytag("img",src="../composite_indicator_visualisation/composite_indicator_graph.jpg")

.scream("Summary Statistics")
seq_along(results) %>% lapply(function(result){
  result<-results[[result]]
  .shout(question_get_question_label(results[[1]]$input.parameters$dependent.var))
  .saytag("img",src=paste0("../barcharts/",result$plotfilename),style="height:80%;width:auto")
})

# ggplot(results$`1`$summary.statistic,aes(independent.var.value,dependent.var.value))+
#   geom_tile(aes(fill=numbers),colour="white")+
#   theme_tufte()+
#   scale_fill_gradient(low = reach_style_color_lightgrey(3),high = reach_style_color_red(1))

}




htmlreport2<-function(results){
  source("./internal/R/htmlR.R")
  results$results<-results$results_unlabeled
  summary_df<-results$results %>% lapply(function(x){x$summary.statistic}) %>% do.call(rbind,.)
  summary_df %>% head
  hierarchical_results<-summary_df %>% 
  (function(x){
    split.data.frame(x,paste("repeat for:",x$repeat.var)) %>% lapply(function(x){
    split.data.frame(x,x$independent.var) %>% lapply(function(x){
    # split.data.frame(x,x$dependent.var) %>% lapply(function(x){
    split.data.frame(x,x$repeat.var.value) %>% lapply(function(x){
      x
      # })
      })
      })
      })
      })

    dir.create("./output/viewer")
  overview.file<- "output/viewer/index.html"
  
  .record(to.file = overview.file)
  .rewind()
  .scream("Overview")
  .saytag("style","div{marign-left:30px;}")
  .scream("Overview")
  
  # (html_hierarchical_list(hierarchical_results,"numbers"))
  .say(print_nested_list(hierarchical_results))
  
  
}
    html_hierarchical_list<-function(hierarchical_list,leaf_to_print){
    # if(is.list(hierarchical_list)&length(hierarchical_list)>0){
      if(is.list(hierarchical_list)){
        # cat(paste("NEXT:",names(hierarchical_list)))      
      lapply(names(hierarchical_list),function(name){
        tag("br")

        cat(name)
        tag("div",html_hierarchical_list(hierarchical_list[[name]],leaf_to_print))

      })
      }else{
      cat(hierarchical_list)
        paste("reached leaf\n<br>")  
        # )
      }
      }
    

    
    
    

print_nested_list<-function(l){
  mystring<-""
  
  for(i in names(l)){
    mystring<-paste(mystring,"\n<br>")
    mystring<-paste(mystring,tag("div",cat(i)))
    mystring<-paste(mystring,tag("br"))
    mystring<-paste(mystring,tag("div",print_nested_list(l[[i]]) %>% cat))
  }
  mystring
}    



