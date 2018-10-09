data <- read.csv(file = "./GC_SYR_Data.csv", stringsAsFactors = F)

independent.var = "Project"
dependent.var = "Q11_Did_the_assistance_provided_meet_your_needs_fall_below_your_needs_or_exceed_them"
design <- map_to_design(data = data)


hypothesis_test_chisquared_select_one(dependent.var, independent.var, design)
summary.statistic <- percent_with_confints_select_one_groups(dependent.var, independent.var, design)
yippie <- grouped_barchart_no_questionnaire(summary.statistic, filename = "barchart.pdf")

grouped_barchart_no_questionnaire <- function(summary.statistic,filename){
  if(is.null(summary.statistic)){return(NULL)}
  if(length(summary.statistic)==0){return(NULL)}
  if(nrow(summary.statistic)==0){return(NULL)}
  if(length(unique(summary.statistic$dependent.var.value))>12){
    warning("I don't do grouped barcharts with more than 12 responses. that's madness! there isn't even 12 colours!")
    return(NULL)}
  percent_formats<-function(x,digits=0){return(paste0(round(x*100,digits),"%"))}
  
  
  
  # summary.statistic<-labels_summary_statistic(summary.statistic)
  theplot<-ggplot(summary.statistic,aes(x=independent.var.value,y=numbers,fill=dependent.var.value))+geom_bar(stat = "identity",position='dodge')+theme_tufte()+
    xlab(NULL)+ylab(NULL)+ 
    theme(text=element_text(family="Arial Narrow"),
          # axis.title.x=element_text(summary.statistic$dependent.var.value"),
          # axis.text.x=element_blank(),
          # axis.ticks.x=element_blank(),
          # axis.title.y=element_blank(),
          # axis.text.y=element_blank(),
          # axis.ticks.y=element_blank(),
          # plot.margin = unit(x = c(0,0,0,0),'null')
    )+scale_fill_reach_categorical(n=length(unique(summary.statistic$dependent.var.value)),name="")+
    scale_y_continuous(limits = c(0,1),labels = scales::percent_format())+
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
<bytecode: 0x000000001cb38698>