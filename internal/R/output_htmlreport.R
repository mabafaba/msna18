

htmlreport<-function(results){
source("./internal/R/htmlR.R")
.saytag<-function(tag,x=NULL,...){
  .say(tag(tag,x,...))
}

.saybold<-function(x){
  tag("b",x)
  
}
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
  .saytag("img",src=paste0("../barcharts/",result$plotfilename))
})

# ggplot(results$`1`$summary.statistic,aes(independent.var.value,dependent.var.value))+
#   geom_tile(aes(fill=numbers),colour="white")+
#   theme_tufte()+
#   scale_fill_gradient(low = reach_style_color_lightgrey(3),high = reach_style_color_red(1))

}
