percent_with_confints <- function(dependent.var,
                                  independent.var,
                                  design,
                                  na.rm = TRUE){



  formula_string<-paste0("~",independent.var, "+",dependent.var )
  f.table <- svytable(formula(formula_string), design)
  p.table <- apply(f.table,1,function(x){x/sum(x)})

  # if(design$variables %>%
  #    split.data.frame(design$variables[[independent.var]]) %>%
  #    lapply(nrow) %>%
  #    unlist %>%
  #    (function(x){x<2}) %>%
  #    any){
  #       warning("independent var must have at least two unique values");return(NULL)
  #     }

  # formula_err <- paste0("~", dependent.var, sep = "")
  # by <- paste0(" ~",independent.var , sep = "")
  # summary.result.svyby <- svyby(formula(formula_err), formula(by), design, na.rm = T, svymean)
  # # fix weird column names:
  # colnames(summary.result.svyby)<-colnames(summary.result.svyby) %>% gsub(paste0("^se.",dependent.var),"",.) %>% gsub(paste0("^",dependent.var),"",.)
  #
  #
  #
  #
  # # if(length(summary.result.svyby)==3 & length(summary.result.svyby[[2]])==2){return(list(ERROR="error calculating convidendce intervals. illformated output attached. might be that variable names too similar to other variable names. Try running again using design object without other variable columns."))}
  # # pick out the columns that are the standard error
  #
  # independent.var.column<-1
  # dependent.var.n<-(ncol(summary.result.svyby)-1)/2
  # stat.columns<-2:(1+dependent.var.n)
  # se.columns<-(max(stat.columns)+1):ncol(summary.result.svyby)
  #
  # standard_error <- summary.result.svyby[,c(independent.var.column,se.columns)] %>% melt(id.vars=c(independent.var))
  # # colnames(standard_error)[which(colnames(standard_error)==independent.var)]<-"independent.var"
  # stat <-         summary.result.svyby[,c(independent.var.column,stat.columns)] %>% melt(id.vars=c(independent.var))
  #
  #
  if(!is.null(p.table) & nrow(p.table)>1){

    p.table %>% melt -> ftable_flipped
    colnames(ftable_flipped)<-c("dependent.var.value","independent.var.value","numbers")
    results<-data.frame(ftable_flipped,se=NA,min=NA,max=NA)
  }else{

  }

  # results<-list(
  #   independent.var.value=ftable
  # )
  # results<-list()
  # results$independent.var.value <- stat[,independent.var]
  # results$dependent.var.value <- stat[,"variable"]
  # results$numbers <-stat[,"value"]
  # results$se <- standard_error[,"value"]
  # results$min <- results$numbers - results$se
  # results$max <- results$numbers + results$se
  # results<-f.table
  return(results)
}






confidence_intervals_num <- function(dependent.var,
                                     independent.var = NULL,
                                     design,
                                     data = data){
    formula_string<-paste0("~as.numeric(", dependent.var, ")")
    summary <- svymean(formula(formula_string), design, na.rm = T)
    confints <- confint(summary, level = 0.95)
    results<-list()
    results$names <- dependent.var
    results$numbers <- summary
    results$min <- confints[,1]
    results$max <- confints[,2]
    return(results)
 }

  confidence_intervals_num_groups <- function(dependent.var,
                                     independent.var,
                                     design,
                                     data){

  formula_string <- paste0("~as.numeric(", dependent.var,")")
  by <- paste0("~", independent.var, sep = "")
  summary <- svyby(formula(formula_string), formula(by), design, svymean, na.rm = T, keep.var = T)
  confints <- confint(summary, level = 0.95)
  summary$min <- confints[,1]
  summary$max <- confints[,2]
  dependent.var.value <- rep(NA, length(summary$min))
  results<- data.frame(dependent.var.value, summary) 
  colnames(results) <- c("dependent.var.value","independent.var.value","numbers", "se", "min", "max")
  return(results)
}

# 
#   percent_with_confints <- f(dependent.var = dependent.var, independent.var = independent.var, design = design)
# 
#     formula_string <- paste0("~as.numeric(", dependent.var,")")
#     by <- paste0("~", independent.var, sep = "")
#     summary <- svyby(formula(formula_string), formula(by), design, svymean, na.rm = T, keep.var = T)
#     confints <- confint(summary, level = 0.95)
#     summary$min <- confints[,1]
#     summary$max <- confints[,2]
#     dependent.var.value <- rep(NA, length(summary$min))
#     results<- data.frame(dependent.var.value, summary)
#     colnames(results) <- c("dependent.var.value","independent.var.value","numbers", "se", "min", "max")
#     return(results)
# }



# dependent.var <- "VAR.18...what.is.your.relationship.to.the.head.of.family."
# independent.var <- "Camp.17"
# design <-  map_to_design(data = data, cluster.var = NULL)

