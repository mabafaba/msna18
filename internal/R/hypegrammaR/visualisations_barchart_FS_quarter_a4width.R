plot_to_file_FS_quarter_a4width<-function(data,filename="test.svg"){
  # make one plot per independent variable value:
  
  
  # some parameters: make sure it's a quarter of an a4 page wide (as in the factsheets):
  A4widthincm<-27.94
  
  smallFSplotwdith<-(A4widthincm/4)
  # set the font size:
  fontsize=9
  # how much plot height per bar?
  heightperbarcm<-0.8
  data$prop<-round(data$numbers*100)
  # data$prop[1]<-100
  # print(data$prop)
  data$min<-data$min*100
  data$max<-data$max*100
  data$min[data$max == data$min]<-NA
  data$max[data$max == data$min]<-NA
  
  # to make sure the labels, numbers and bars behave nicely and don't overlap, I've split the plot in three, then arranging them with a grid.
  #### only bars with no labels at all
  plot_bars <- function(data){
    ggplot(data, aes(x = reorder(dependent.var.value,prop), y = prop, width=0.8)) +
      geom_bar(stat = "identity", fill= reach_style_color_red(),position = position_nudge(y = 0,x=0))+theme_tufte()+
      geom_errorbar( aes(x=dependent.var.value,
                         ymin=as.numeric(min),
                         ymax=as.numeric(max)),
                     position=position_dodge(),
                     stat='identity',
                     width=0.2,na.rm = T)+
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
              element_text(size  = fontsize,
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
      geom_text(aes(label=paste0(round(prop),"%")),size = fontsize * 0.352777,family="Arial Narrow",position = position_stack(vjust = 0),hjust=0)+
      coord_flip()+
      theme(plot.margin = unit(c(0,0,0,0), "cm"))
  }
  
  fullplot<-grid.arrange(plot_labels(data),
                         plot_numbers(data),
                         plot_bars(data), ncol=3,widths=smallFSplotwdith*c(0.6,0.09,0.31))
  
  ggsave(file=filename, plot=fullplot,width =smallFSplotwdith, height=0.4+heightperbarcm*length(unique(data$independent.var)),units = "cm",device = "jpeg")   
  
  return(fullplot)
}