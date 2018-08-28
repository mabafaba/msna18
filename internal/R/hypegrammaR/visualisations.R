
grouped_barchart_percent<-function(summary.statistic,filename){
  if(is.null(summary.statistic)){return(NULL)}
  if(length(summary.statistic)==0){return(NULL)}
  if(nrow(summary.statistic)==0){return(NULL)}
  if(length(unique(summary.statistic$dependent.var.value))>12){
    warning("I don't do grouped barcharts with more than 12 responses. that's madness! there isn't even 12 colours!")
    return(NULL)}
  percent_formats<-function(x,digits=0){return(paste0(round(x*100,digits),"%"))}
  
  
  
  summary.statistic<-labels_summary_statistic(summary.statistic)
  theplot<-ggplot(summary.statistic,aes(x=independent.var.value,y=numbers,fill=dependent.var.value))+geom_bar(stat = "identity",position='dodge')+theme_tufte()+
    xlab("")+ylab("percent")+ 
    theme(text=element_text(family="Arial Narrow"),
          # axis.title.x=element_text(summary.statistic$dependent.var.value"),
          # axis.text.x=element_blank(),
          # axis.ticks.x=element_blank(),
          # axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          # axis.ticks.y=element_blank(),
          # plot.margin = unit(x = c(0,0,0,0),'null')
    )+scale_fill_reach_categorical(n=length(unique(summary.statistic$dependent.var.value)),name="")+
    geom_errorbar( aes(x=summary.statistic$independent.var.value,
                       ymin=as.numeric(summary.statistic$min),
                       ymax=as.numeric(summary.statistic$max)),
                   position=position_dodge(width=0.9),
                   stat='identity',
                   width=.1)+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  number_of_bars<-(length(unique(summary.statistic$dependent.var.value))*length(unique(summary.statistic$independent.var.value)))
  plotwidth<-5+number_of_bars*1.5
  map_to_file(theplot,filename,height=12,width=plotwidth,unit="cm")
  # 
  return(theplot)
}




barchart_average<-function(summary.statistic,filename){
  summary.statistic$min[summary.statistic$min<0]<-0
  summary.statistic<-labels_summary_statistic(summary.statistic)
  theplot<-ggplot(summary.statistic,aes(x=independent.var.value,y=numbers),fill=reach_style_color_darkgrey(1))+geom_bar(stat = "identity")+theme_tufte()+
    xlab("")+ylab(summary.statistic$dependent.var[1])+ 
    theme(text=element_text(family="Arial Narrow"),
          axis.title.x=element_blank(),
          # axis.text.x=element_text(angle=90),
          # axis.ticks.x=element_blank(),
          axis.title.y=element_blank()
          # axis.text.y=element_blank(),
          # axis.ticks.y=element_blank(),
          # plot.margin = unit(x = c(0,0,0,0),'null')
    )+
    geom_errorbar( aes(x=summary.statistic$independent.var.value,
                       ymin=as.numeric(summary.statistic$min),
                       ymax=as.numeric(summary.statistic$max)),
                   stat='identity',
                   width=.1) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
  
  number_of_bars<-(length(unique(summary.statistic$independent.var.value)))
  plotwidth<-5+(number_of_bars*1.5)
  map_to_file(theplot,filename,height=12,width=plotwidth,unit="cm")
  return(theplot)
}


# make_nogroup<-function(summary.statistic){return(summary.statistic[summary.statistic$dependent.var.value==summary.statistic$dependent.var.value[1],])}

barchart_percent<-function(summary.statistic,filename){
  summary.statistic<-labels_summary_statistic(summary.statistic)
  theplot<-ggplot(summary.statistic,aes(x=dependent.var.value,y=numbers),fill=reach_style_color_darkgrey(1))+geom_bar(stat = "identity")+theme_tufte()+
    xlab("")+ylab(summary.statistic$dependent.var[1])+ 
    theme(text=element_text(family="Arial Narrow"),
          axis.title.x=element_blank(),
          # axis.text.x=element_blank(),
          # axis.ticks.x=element_blank(),
          axis.title.y=element_blank()
          # axis.text.y=element_blank(),
          # axis.ticks.y=element_blank(),
          # plot.margin = unit(x = c(0,0,0,0),'null')
    )+scale_y_continuous(limits = c(0,1),labels = scales::percent_format())+
    geom_errorbar( aes(x=summary.statistic$dependent.var.value,
                       ymin=as.numeric(summary.statistic$min),
                       ymax=as.numeric(summary.statistic$max)),
                   stat='identity',
                   width=.1) + theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5)) 
  
  number_of_bars<-(length(unique(summary.statistic$dependent.var.value)))
  plotwidth<-5+(number_of_bars*1.5)
  map_to_file(theplot,filename,height=12,width=plotwidth,unit="cm")
  return(theplot)
}





visualisation_barchart_percent_nogroups_FS<-function(data,filename="test.svg"){
  # some parameters: make sure it's a quarter of an a4 page wide (as in the factsheets):
  A4widthincm<-27.94
  smallFSplotwdith<-(A4widthincm/4)
  # set the font size:
  fonsize=8
  # how much plot height per bar?
  heightperbarcm<-0.6
  
  data$prop<-data$numbers*100
  
  # to make sure the labels, numbers and bars behave nicely and don't overlap, I've split the plot in three, then arranging them with a grid.
  #### only bars with no labels at all
  plot_bars <- function(data){
    ggplot(data, aes(x = reorder(dependent.var.value,prop), y = prop, width=0.5)) +
      geom_bar(stat = "identity", fill= reach_style_color_red(),position = position_nudge(y = 5,x=0))+theme_tufte()+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.ticks.length = unit(0, "mm"))+
      coord_flip()+
      theme(plot.margin = unit(c(0,0,0,0), "cm"))+
      ylim(c(0,100))
    
  }
  
  # font size units different in theme() and in geom_text(): factor 1/0.35
  # only labels with no bars at all
  plot_labels<-function(data){
    ggplot(data, aes(x = reorder(dependent.var.value,prop), y = prop, width=0.5)) +
      # geom_bar(stat = "identity", fill= reach_style_color_red(),position = position_nudge(y = 5,x=0))+
      theme_tufte()+
      theme(axis.text.x = element_blank(), axis.ticks.x=element_blank(),axis.title.x=element_blank(),text =element_text(family="Arial Narrow"))+
      theme(axis.title.y=element_blank(),axis.ticks.y=element_blank(),text =element_text(family="Arial Narrow"))+
      theme(text=element_text(family="Arial Narrow"))+
      coord_flip()+
      theme(axis.text.y =
              element_text(size  = fonsize,
                           angle = 0,
                           hjust = 0,
                           vjust = 0.5,
                           colour = "black",
                           family = "Arial Narrow"),
            axis.ticks.length = unit(0, "mm"))+
      theme(plot.margin = unit(c(0,0,0,0), "cm"))
    
  }
  # only numbers with none of the other stuff
  plot_numbers<-function(data){
    ggplot(data, aes(x = reorder(dependent.var.value,prop), y = prop, width=0.5)) +
      # geom_bar(stat = "identity", fill= reach_style_color_red(),position = position_nudge(y = 5,x=0))+
      theme_tufte()+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.ticks.length = unit(0, "mm"))+
      geom_text(aes(label=paste0(round(prop),"%")),size = fonsize * 0.352777,family="Arial Narrow",position = position_stack(vjust = 0),hjust=0)+
      coord_flip()+
      theme(plot.margin = unit(c(0,0,0,0), "cm"))
  }
  
  fullplot<-grid.arrange(plot_labels(data),
                         plot_numbers(data),
                         plot_bars(data), ncol=3,widths=smallFSplotwdith*c(0.4,0.1,0.5))
  
  ggsave(file=filename, plot=fullplot,width =smallFSplotwdith, height=heightperbarcm*length(unique(data$dependent.var.value)),units = "cm",device = "jpg",limitsize = F)   
  return(fullplot)
}





