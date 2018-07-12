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

  return(results)}

aggregate_percent_select_multiple <- function(dependent.var,
                                              independent.var,
                                              data,
                                              design,
                                              na.rm = TRUE){
  
  var_list <- grep(paste0(dependent.var, "."), names(data))
  datalist = list()

  for(i in var_list) {
    formula_string <- paste0("~", independent.var, "+", names(data)[i])
    f.table <- svytable(formula(formula_string), design)
    if(ncol(f.table) > 1) {
      p.table <- apply(f.table, 1, function(x) {x/sum(x)})
      p.table[is.nan(p.table)] <- NA
      p.names <- colnames(p.table)
      p.table <- p.table["1",]
      p.table <- cbind(p.table, colnames(p.table))
    }
    
    else if(ncol(f.table) == 1) {
      p.table <- f.table[,1]
      p.table[f.table[,1] == 0] <- NA
      p.table[f.table[,1] > 0] <- as.numeric(colnames(f.table))
    }
    p.table.val <- p.table
    p.table.val <- names(data)[i]
    p.table <- cbind(p.table.val, rownames(f.table), p.table)
    p.table <- data.frame(p.table, se = NA, min = NA, max = NA)
    names(p.table) <- c("dependent.var.value", "independent.var.value", "numbers", "se", "min", "max")
    
    datalist[[match(i, var_list)]] <- p.table
  }
  
  p.table <- bind_rows(datalist)
  p.table <- p.table[order(p.table[,2]),]
  row.names(p.table) <- seq(nrow(p.table))
  return(p.table)}
