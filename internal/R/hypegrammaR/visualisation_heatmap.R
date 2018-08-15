


heatmap_to_file<-function(summary.statistic,filename){
theplot<-ggplot(summary.statistic)+
  geom_tile(aes(x=dependent.var.value,y=independent.var.value,fill=numbers))+
  theme_tufte()+theme(text = element_text(family = "Arial Narrow"))+
  scale_fill_gradient(low=reach_style_color_lightgrey(1),high=reach_style_color_red(1))+xlab("")+ylab("")+coord_fixed()


  # plot<-ggplot(allstats %>% as.data.frame)+
  #   geom_tile(aes(x=repeat.var.value,y=dependent.var.value,fill=numbers))+
  #   theme_tufte()+theme(text = element_text(family = "Arial Narrow"))+scale_fill_gradient(low=reach_style_color_lightgrey(1),high=reach_style_color_red(1))
  ggsave(filename = filename,plot = theplot,units = "cm",width = 10+0.5*length(unique(summary.statistic$dependent.var.value)),height = 10+0.5*length(unique(summary.statistic$independent.var.value)))

  
# ggplot(allstats %>% as.data.frame)+
#   geom_tile(aes(x=repeat.var.value,y=dependent.var.value,fill=numbers))+
#   theme_tufte()+theme(text = element_text(family = "Arial Narrow"))+
#   scale_fill_gradient(low=reach_style_color_lightgrey(1),high=reach_style_color_red(1))+facet_wrap(~dependent.var)


}


