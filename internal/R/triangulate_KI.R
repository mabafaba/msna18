
triangulate<-function(data,by.var.name,vartypes=NULL){
  lapply(names(data),function(varname){
    triangulation_fun<-map_to_triangulation(varname)
    return(triangulation_fun(data[,varname],by=by.var.name))
  }) %>% as.data.frame(stringsAsFactors=F)
}





triangulate_mode<-function(x,by){
  
  triangulated <-  split(x,by) %>% lapply(Mode) %>% unlist(use.names = T)
  # triangulated_vector<- triangulated[["x"]]
  # names(triangulated_vector)<-triangulated[["Group.1"]]
  # triangulated_vector
}

triangulate_combine_select_multiple<-function(x,by){
  split(x,by) %>% lapply(function(x){strsplit(x," ") %>% unlist %>% unique %>% paste(collapse =" ")})
}

numeric_consensus<-function(x){
  # accepting 30% difference at most
  if((max(x)-min(x))/min(x)>0.3){return(FALSE)}
  TRUE
}


triangulate_mean<-function(x,by){
  split(as.numeric(x),by) %>% lapply(
    function(x){
      if(!numeric_consensus(x)){return(NA)}
      return(mean(x))
    }) %>% unlist
}

triangulate_empty<-function(x,by){
  empty<-rep(NA,length(unique(by)))
  names(empty)<-unique(by)
  empty
}


map_to_triangulation<-function(varname=NULL,vartype=NULL){
  
  if(is.null(varname) & is.null(vartype)){stop("must provide at least one of 'varname' or 'vartype' parameters")}

  if(is.null(vartype)){
    vartype<-question_variable_type(varname)
    }
  if(!(vartype%in% c("select_one","select_multiple","numeric"))){
    warning(paste(
      "don't know how to triangulate variable of type: '",
      vartype,
      "'. Must be one of 'select_one', 'select_multiple', 'numeric'. Triangulating to NA."))
    return(triangulate_empty)
  }
  
  
  if(vartype=="select_multiple"){return(triangulate_combine_select_multiple)}
  if(vartype=="select_one"){return(triangulate_mode)}
  if(vartype=="numeric"){return(triangulate_mean)}
}





