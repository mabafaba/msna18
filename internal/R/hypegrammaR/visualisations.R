# reach_style_barchart<-function(group,percent,error_min=NULL,error_max=NULL,horizontal=T){
#   require('extrafont')
#   require('ggplot2')
#   require("ggthemes")
#   require("dplyr")
#   require('grid')
#   
#   percent_formats<-function(x,digits=0){return(paste0(round(x*100,digits),"%"))}
#   
#   df<-data.frame(group=group,percent=percent,ymin=error_min,ymax=error_max)
#   
#   colnames(df)<-c('group','percent','ymin','ymax')
#   
#   theplot<-
#     
#     ggplot(df)+
#     geom_bar(aes(y=1,x=group),
#              stat='identity',
#              fill=reach_style_color_lightgrey()) +
#     geom_bar(aes(y=percent,x=group),
#              stat='identity',
#              fill=reach_style_color_red()) +
#     
#     geom_errorbar( aes(x=group,
#                        y=percent,
#                        ymin=ymin,
#                        ymax=ymax),
#                    stat='identity',
#                    width=0) +
#     
#     geom_text(aes(x=group, y=(1), label=paste0("  ", round(percent*100),"%"," ",group)),size=4,
#               family="Arial Narrow",
#               col='#000000',
#               hjust=0,
#               vjust=0.5) +
#     
#     theme_tufte()+reachR:::reach_theme() +
#     theme(text=element_text(family="Arial Narrow"),
#           axis.title.x=element_blank(),
#           axis.text.x=element_blank(),
#           axis.ticks.x=element_blank(),
#           axis.title.y=element_blank(),
#           axis.text.y=element_blank(),
#           axis.ticks.y=element_blank(),
#           plot.margin = unit(x = c(0,0,0,0),'null')) +
#     
#     scale_y_continuous(labels = percent_formats,limits=c(0,3))
#   
#   if(horizontal){
#     theplot<-theplot+coord_flip()}
#   return(theplot)
# }
# 


# barchart_with_error_bars <- function(hypothesis.test.results,summary.statistics){
#   test_name <- hypothesis.test.results$test.parameters[[3]]
#   p_value <- hypothesis.test.results$test.results[[2]]
#   
#   chart <- reach_style_barchart(group = summary.statistics$independent.var.value, 
#                                 percent = summary.result$numbers, 
#                                 error_min = summary.result$min, 
#                                 error_max =  summary.result$max)
#   
#   chart + geom_text(aes(x =4, 
#                         y = 2,
#                         label= paste0("To determine ", hypothesis.type, "\n", test_name, "\n"
#                                       ," returned a p value of ", round(p_value,6))),
#                     size=3,
#                     family="Arial Narrow",
#                     col='#000000',
#                     hjust=0,
#                     vjust=0.5)
#   }


grouped_barchart_percent<-function(summary.statistic,filename){
  if(length(summary.statistic)==0){return(NULL)}
  if(nrow(summary.statistic)==0){return(NULL)}
  if(length(unique(summary.statistic$dependent.var.value))>12){
    warning("I don't do grouped barcharts with more than 12 responses. that's madness! there isn't even 12 colours!")
    return(NULL)}
  percent_formats<-function(x,digits=0){return(paste0(round(x*100,digits),"%"))}
  theplot<-ggplot(summary.statistic,aes(x=independent.var.value,y=numbers,fill=dependent.var.value))+geom_bar(stat = "identity",position='dodge')+theme_tufte()+
    xlab(unique(summary.statistic$independent.var)[1])+ylab("percent")+ 
    theme(text=element_text(family="Arial Narrow")
          # axis.title.x=element_text(summary.statistic$dependent.var.value"),
          # axis.text.x=element_blank(),
          # axis.ticks.x=element_blank(),
          # axis.title.y=element_blank(),
          # axis.text.y=element_blank(),
          # axis.ticks.y=element_blank(),
          # plot.margin = unit(x = c(0,0,0,0),'null')
          )+scale_fill_reach(name=unique(summary.statistic$dependent.var)[1])+
    geom_errorbar( aes(x=summary.statistic$independent.var.value,
                       ymin=as.numeric(summary.statistic$min),
                       ymax=as.numeric(summary.statistic$max)),
                   position=position_dodge(width=0.9),
                   stat='identity',
                   width=.1)+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
 
  number_of_bars<-5+(length(unique(summary.statistic$dependent.var.value)*length(unique(summary.statistic$independent.var.value))))*1.5
  map_to_file(theplot,filename,width=)
    # 
  return(theplot)
  }




barchart_average<-function(summary.statistic,filename){
  
  
  theplot<-ggplot(summary.statistic,aes(x=independent.var.value,y=numbers),fill=reach_style_color_darkgrey(1))+geom_bar(stat = "identity")+theme_tufte()+
    xlab(unique(summary.statistic$independent.var)[1])+ylab(summary.statistic$dependent.var[1])+ 
    theme(text=element_text(family="Arial Narrow")
          # axis.title.x=element_text(summary.statistic$dependent.var.value"),
          # axis.text.x=element_blank(),
          # axis.ticks.x=element_blank(),
          # axis.title.y=element_blank(),
          # axis.text.y=element_blank(),
          # axis.ticks.y=element_blank(),
          # plot.margin = unit(x = c(0,0,0,0),'null')
    )+
    geom_errorbar( aes(x=summary.statistic$independent.var.value,
                       ymin=as.numeric(summary.statistic$min),
                       ymax=as.numeric(summary.statistic$max)),
                   stat='identity',
                   width=.1) 
  
  
  map_to_file(theplot,filename)
  # 
  return(theplot)
}












