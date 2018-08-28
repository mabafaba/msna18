
map_to_visualisation_heatmap<-function(case){
  # iscat<-grepl("categorical",case)
  # list_all_cases(implemented_only = T) %>% paste0("if(grepl(\"",,"\"","){return()}\n") %>% cat
  if(grepl("_categorical_categorical",case)){return(heatmap_categorical_to_file)}
  if(grepl("_numerical_",case)){return(heatmap_numerical_to_file)}
  if(grepl("categorical_$",case)){return(heatmap_categorical_nogroups_to_file)}
 
  return(function(...){return(NULL)})
}



gg_heatmap_generic<-function(summary.statistic){
  summary.statistic_labeled<-labels_summary_statistic(summary.statistic,label.dependent.var.value = T,label.independent.var.value = T)
  summary.statistic_labeled<-summary.statistic_labeled[order(summary.statistic_labeled$numbers),]
  theplot<-ggplot(summary.statistic_labeled)+
    geom_tile(aes(x=dependent.var.value,y=independent.var.value,fill=numbers))+
    xlab("")+ylab("")+coord_fixed()
    theplot %<>%  gg_reach_style()
  return(theplot)
}


heatmap_categorical_to_file<-function(summary.statistic,filename){
  
  theplot<-gg_heatmap_generic(summary.statistic)+reach_style_scale_fill_gradient_percent()
  ggsave(filename = filename,
         plot = theplot,
         units = "cm",
         width = 10+0.5*length(unique(summary.statistic$dependent.var.value)),
         height = 10+0.5*length(unique(summary.statistic$independent.var.value)),
         limitsize = F)
  return(theplot)
  }



heatmap_categorical_nogroups_to_file<-function(summary.statistic,filename){
  theplot<-gg_heatmap_generic(summary.statistic)+reach_style_scale_fill_gradient_percent()+
  theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())+
  # coord_flip()+
  theme(aspect.ratio = length(unique(summary.statistic$independent.var.value))/
    length(unique(summary.statistic$dependent.var.value)))
          
  ggsave(filename = filename,
         plot = theplot,
         units = "cm",
         width = 10+0.5*length(unique(summary.statistic$dependent.var.value)),
         height = 10+0.5*length(unique(summary.statistic$independent.var.value)),
         limitsize = F)
  return(theplot)
  }


heatmap_numerical_to_file<-function(summary.statistic,filename){
  theplot<-gg_heatmap_generic(summary.statistic)+
    reach_style_scale_fill_gradient_numeric()+
    theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())+
    coord_flip()+
    theme(aspect.ratio = length(unique(summary.statistic$dependent.var.value))/length(unique(summary.statistic$independent.var.value))
            )
  
  ggsave(filename = filename,
         plot = theplot,
         units = "cm",
         width = 10+0.5*length(unique(summary.statistic$dependent.var.value)),
         height = 10+0.5*length(unique(summary.statistic$independent.var.value)),
         limitsize = F)
  return(theplot)
}


reach_style_scale_fill_gradient_numeric<-function(...){
  scale_fill_gradient(
    name="",
    position="top",
    low=reach_style_color_darkgrey(1),
    high=reach_style_color_red(1),
    guide = "legend",
    ...
  )  
}


reach_style_scale_fill_gradient_percent<-function(...){
  scale_fill_gradient(
    name="",
    position="top",
    low=reach_style_color_darkgrey(1),
    high=reach_style_color_red(1),
    limits=c(0,1),
    guide = "legend",
    labels = scales::percent_format(),
    ...
  )  
}

gg_reach_style<-function(ggobject){
  return(ggobject+theme_tufte()+theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5),text = element_text(family = "Arial Narrow")))
}
  
