


percent_with_confints <- function(dependent.var,
                                  independent.var,
                                  design,
                                  na.rm = TRUE){


  # if dependent and independent variables have only one value, just return that:
  
  if(length(unique(data[[dependent.var]]))==1){
    dependent.var.value=unique(data[[dependent.var]])
    if(length(unique(data[[independent.var]]==1))){
      independent.var.value=unique(data[[independent.var]])	
      return(data.frame(dependent.var,independent.var,dependent.var.value,independent.var.value,numbers=1,se=NA,min=NA,max=NA))
      
    }
  }
  
  # formula_summary<-paste0("~",dependent.var, "+",independent.var )
  # f.table <- svytable(formula(formula_summary), design)
  # p.table <- apply(f.table,1,function(x){x/sum(x)})
  # table(design$variables[[dependent.var]],design$variables[[independent.var]])
  
  formula_string <- paste0("~",dependent.var,sep = "")
  by <- paste0("~", independent.var, sep = "")
  
  result_hg_format<- # tryCatch(
    {
    result_svy_format <- svyby(formula(formula_string), formula(by), design, svymean, na.rm = T, keep.var = T,vartype = "ci")
    unique.dependent.var.values<- design$variables[[dependent.var]] %>% unique
    summary_with_confints<-unique.dependent.var.values %>%
      lapply(function(x){
        summary_stat_colname<-paste0(dependent.var,x)
        lower_confint_colname<-paste0("ci_l.",summary_stat_colname)
        upper_confint_colname<-paste0("ci_u.",summary_stat_colname)
        
        dependent_value_x_stats<-result_svy_format[,c(independent.var,summary_stat_colname,lower_confint_colname,upper_confint_colname)]
        colnames(dependent_value_x_stats)<-c("independent.var.value","numbers","min","max")
        data.frame(dependent.var=dependent.var,
                   independent.var=independent.var,
                   dependent.var.value=x,
                   independent.var.value=dependent_value_x_stats[,"independent.var.value"],
                   numbers=dependent_value_x_stats[,"numbers"],
                   se=NA,
                   min=dependent_value_x_stats[,"min"],
                   max=dependent_value_x_stats[,"max"])
      }) %>% do.call(rbind,.)
    
    
    summary_with_confints[,"min"] <- summary_with_confints[,"min"] %>% replace(summary_with_confints[,"min"] < 0 , 0)
    summary_with_confints[,"max"] <- summary_with_confints[,"max"] %>% replace(summary_with_confints[,"max"] > 1 , 1)
    summary_with_confints %>% as.data.frame
     }
    #,
    # error=function(cond){        data.frame(dependent.var=NA,
    #                                         independent.var=NA,
    #                                         dependent.var.value=NA,
    #                                         independent.var.value=NA,
    #                                         numbers=NA,
    #                                         se=NA,
    #                                         min=NA,
    #                                         max= NA)[FALSE,]
    #                    }
    #                  )
  
   
  return(result_hg_format)
    }
    
      
  
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
  # stat <-         summary.result.svyby[,c(independent.var.column,)] %>% melt(id.vars=c(independent.var))
  #
  #

  
  # check if we actually got  a frequency table back; problems can arise here if independent.var has only 1 unique value 
  # if(!(nrow(as.data.frame(p.table)>1))){stop("DEV: unexpected edge case in percent_with_confints - freq table has 1 or less rows. contact development team about this error.")}
  # 
  #   p.table %>% melt -> ftable_flipped
  # 
  #   colnames(ftable_flipped)<-c("dependent.var.value","independent.var.value","numbers")
  #   results<-data.frame( dependent.var = dependent.var,
  #                        independent.var = independent.var,
  #                        ftable_flipped,
  #                        se=NA,
  #                        min=confints[,1],
  #                        max=confints[,2])

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
#   return(results)
# }



confidence_intervals_mean <- function(dependent.var,
                                     independent.var = NULL,
                                     design,
                                     data = data){
    if(!is.null(independent.var)){warning("confidence intervals calculated without disaggregation, but received data for an independent variable.")}
    formula_string<-paste0("~as.numeric(", dependent.var, ")")
    summary <- svymean(formula(formula_string), design, na.rm = T)
    confints <- confint(summary, level = 0.95)
    results<-list()
    results$names <- dependent.var
    results$numbers <- summary
    results$min <- confints[,1]
    results$max <- confints[,2]
    return(results %>% as.data.frame)
 }

  confidence_intervals_mean_groups <- function(dependent.var,
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
  results<- data.frame(dependent.var,independent.var,dependent.var.value, summary) 
  colnames(results) <- c("dependent.var","independent.var","dependent.var.value","independent.var.value","numbers", "se", "min", "max")
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

