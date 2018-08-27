---
title: "MSNA Analysis Results"
date: Sys.Date()
output: 
  html_document:
    theme: yeti 
    toc: true
    toc_float: true
    number_sections: true
    self_contained: true

---

```{r,echo = F, warning=FALSE, message=FALSE, results='asis' }

library(knitr)
opts_knit$set(progress = FALSE, verbose = FALSE)
opts_chunk$set(warning=FALSE, message=FALSE, echo=FALSE)

`%pull%`<-function(list,item){lapply(list,function(x){x[[item]]})}


proper<-function(s) sub("(.)", ("\\U\\1"), tolower(gsub("_"," ",s)), pe=TRUE)
coerc<-function(x){as.numeric(chr(x))}


heatmap_oc<-function(datag,case,fsize=6,is_ordered=FALSE,font_family="Arial Narrow",H_col="#A7A9AC"){
  
  # here can percentage
  fct<-100
  
  datag$disa<-ifelse(is.na(datag$repeat.var.value),
                     as.character(datag$independent.var.value),
                     paste0(datag$repeat.var.value,": ",datag$independent.var.value)
  ) %>% proper
  
    if(case!="CASE_group_difference_categorical_categorical"){
      datag$cl_lw<-coerc(datag$min)
      datag$cl_up<-coerc(datag$max)
    }else{
      datag$cl_lw<-coerc(datag$min)*fct
      datag$cl_up<-coerc(datag$max)*fct
    }
  
    if(case!="CASE_group_difference_categorical_categorical"){
      datag$value<-coerc(datag$numbers)
    }else{
      datag$value<-coerc(datag$numbers)*fct
    }
  
    
    datag$variable<-proper(factor(datag$dependent.var.value,levels=rev(levels(factor(datag$dependent.var.value)))))
    
    p <- ggplot(datag, aes(x=datag$disa, y=datag$variable)) 
    p<-p+geom_tile(aes(fill = datag$value),colour = "white")
    if(case!="CASE_group_difference_categorical_categorical"){
      p<-p+geom_text(aes(label=paste(round(datag$value,2)," (",round(datag$cl_lw,2)," , ",round(datag$cl_up,2),")",sep="")), family=font_family,size = fsize,color="#42423E")
    }else{
      p<-p+geom_text(aes(label=paste(round(datag$value,1),"% (",round(datag$cl_lw,1)," , ",round(datag$cl_up,1),")",sep="")), family=font_family,size = fsize,color="#42423E")
    }

    p<-p+scale_fill_gradient(low = "white",high = H_col )+
      scale_x_discrete(expand = c(0, 0)) +
      xlab("") + 
      ylab("") 
    
    p.bot<-p+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(fill=NA,color="white", size=0.5, linetype="solid"),
                    axis.line = element_blank(),
                    axis.ticks = element_blank(),
                    panel.background = element_rect(fill="white"),
                    plot.background = element_rect(fill="white"),
                    legend.position = "none", 
                    axis.text.x = element_blank(),
                    plot.margin = unit(c(1,0,0,0), "cm"),
                    axis.text.y=element_text(family=font_family,size=round(fsize*3.4,0),color="#42423E"))
    
    p.top   <-  p.bot + theme(
      axis.text.x = element_text(family=font_family,size=round(fsize*3.4,0)),
      axis.text.y = element_text(family=font_family,size=round(fsize*3.4,0),color="white")
    )  + coord_cartesian(ylim = c(0,0))
    
    require(gtable)
    #Extract Grobs
    g1<-ggplotGrob(p.top)
    g2<-ggplotGrob(p.bot)
    #Bind the tables
    g<-gtable:::rbind_gtable(g1, g2, "first")
    #Remove a row between the plots
    g <- gtable_add_rows(g, unit(-1.25,"cm"), pos=nrow(g1))
    #draw
    panels <- g$layout$t[grep("panel", g$layout$name)]
    g$heights[panels] <- unit.c(unit(0,"null"),unit(2,"null"))
    grid.newpage()
    grid.draw(g)
}



# terrible loop
for (mycount in 1:length(results$results)){

  
  if(is.na(results$results[[mycount]]$input.parameters$repeat.var)){
      titl<-paste0(" by ",
              results$results[[mycount]]$input.parameters$independent.var,
              ": ",
              results$results[[mycount]]$input.parameters$dependent.var)
    
  }else{
    titl<-paste0(" by ",
              results$results[[mycount]]$input.parameters$independent.var,
              ": ",
              results$results[[mycount]]$input.parameters$dependent.var,
              " / ",
               results$results[[mycount]]$input.parameters$repeat.var,
              "=",
               results$results[[mycount]]$input.parameters$repeat.var.value)
  }
  
  cat(paste("\n\n\n\n# ",titl,"\n\n\n\n"))
    
    
  if(results$results[[mycount]]$message=="success (or unidentified issue)"){
    
    case<-results$results[[mycount]]$`input.parameters`$case
    
    df<-results$results[[mycount]]$summary.statistic[,c(
      "repeat.var.value",
      "dependent.var.value",
      "independent.var.value",
      "numbers",
      "se",
      "min",
      "max")]
    
    kdf<-df
    
    # format for export
    names(kdf)<-c(
      results$results[[mycount]]$input.parameters$repeat.var,
      results$results[[mycount]]$input.parameters$dependent.var,
      results$results[[mycount]]$input.parameters$independent.var,
      "numbers",
      "se",
      "CI_lw",
      "CI_up"
      )
    
    kdf<-kdf[,which(lapply(kdf,function(x){!all(is.na(x))}) %>% unlist)]
    
    
    if(case=="CASE_group_difference_numerical_categorical" & !is.null(results$results[[mycount]]$hypothesis.test$name)){
      cat_stat<-paste0(results$results[[mycount]]$hypothesis.test$name,': p=',round(results$result[[mycount]]$hypothesis.test$result$p.value[1],3),
                       '; ',paste0('df=',round(results$results[[mycount]]$hypothesis.test$parameters$df,1)))
    } else if(case=="CASE_group_difference_categorical_categorical" & !is.null(results$results[[mycount]]$hypothesis.test$name)){
      cat_stat<-paste0(results$results[[mycount]]$hypothesis.test$name,': p=',round(results$results[[mycount]]$hypothesis.test$results$p.value[1],3),
                       '; ',paste0('ddf=',round(results$results[[mycount]]$hypothesis.test$parameters$ddf,1)))
    }else {
      cat_stat<-"statistics test failed"
    }
    
    
    	fig_n<- paste0("figure_",mycount)
    	ht<-round(2+length(unique(paste0(df$repeat.var.value,df$dependent.var.value)))*0.6,1)
    	lg<-round(6+length(unique(df$independent.var.value))*2,0)
    
    
    cat(knit(text = knit_expand(text = 
    			 sprintf("```{r %s, fig.height=%s, fig.width=%s, results='asis'  }\n	
    				\n
    				cat('\n\n\n## Graph \n\n') 
    				\n
            heatmap_oc(df,case)
    				\n\n\n
    				cat('\n\n\n## Table \n\n') 
    				\n
    				kable(kdf)
    				\n\n\n
            
    				cat('\n\n\n## Statistics \n\n') 
            cat(cat_stat)
    				\n\n\n
    				\n\n\n


    			 ```", fig_n, ht, lg)
    		)))	
  } else {
    
    cat(results$results[[mycount]]$message)
    
  }
}


```



