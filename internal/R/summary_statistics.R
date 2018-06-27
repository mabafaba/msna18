aggregate_percent_select_one <- function(dependent.var,
                                  independent.var,
                                  design,
                                  na.rm = TRUE){
  
  
  
  formula_string<-paste0("~",independent.var, "+",dependent.var )
  f.table <- svytable(formula(formula_string), design)
  p.table <- apply(f.table,1,function(x){x/sum(x)})
 
  #
  if(!is.null(p.table) & nrow(p.table)>1){
    
    p.table %>% melt -> ftable_flipped
    colnames(ftable_flipped)<-c("dependent.var.value","independent.var.value","numbers")
    results<-data.frame(ftable_flipped,se=NA,min=NA,max=NA)
  }else{
    
  }

  return(results)
}