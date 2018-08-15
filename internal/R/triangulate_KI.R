## Outside function that returns the data triangulated to the level specified in the input sheet
## calls map_to_triangulation to decide which triangulation method to use
triangulate<-function(data,by.var.name,vartypes=NULL){
  lapply(names(data),function(varname){
    triangulation_fun<-map_to_triangulation(varname)
    return(triangulation_fun(data[,varname],by=data[,by.var.name]))
  }) %>% as.data.frame(stringsAsFactors=F)
}


######################## 
# Triangulation functions
######################## 

triangulate_mode<-function(x,by){
  triangulated <-  split(x,by) %>% lapply(mode) %>% unlist(use.names = T)
  # triangulated_vector<- triangulated[["x"]]
  # names(triangulated_vector)<-triangulated[["Group.1"]]
  # triangulated_vector
}

triangulate_combine_select_multiple<-function(x,by){
  split(x,by) %>% lapply(function(x){strsplit(x," ") %>% unlist %>% unique %>% paste(collapse =" ")})
}


triangulate_mean<-function(x,by){
  split(as.numeric(x),by) %>% lapply( 
    function(x){
      if(!is.null(x) & !is.na(x)){
      if(!numeric_consensus(x)){return(NA)} 
      return(mean(x))}else{return(NA)}
    }) %>% unlist
}

triangulate_empty<-function(x,by){
  empty<-rep(NA,length(unique(by)))
  names(empty)<-unique(by)
  empty
}

numeric_consensus<-function(x){
  # accepting 30% difference at most
  if(sum(x, na.rm = T) != 0){
  if(((max(x, na.rm = T) - min(x, na.rm = T))/min(x, na.rm = T))> 0.5){return(FALSE)}}
  TRUE
}

######################## 
# Choosing between triangulation methods
######################## 


map_to_triangulation<-function(varname=NULL){ #vartype as a possible input could be built in again, took it out because it threw an error
  vartype <- question_variable_type(varname)
 
   if(is.null(varname)){stop("must provide at least one of 'varname' or 'vartype' parameters")}

  if(!(vartype%in% c("select_one","select_multiple","numeric"))){
    warning(paste(
      "don't know how to triangulate variable of type: '",
      vartype,
      "'. Must be one of 'select_one', 'select_multiple', 'numeric'. Triangulating to NA."))
    return(triangulate_empty)
  }
  
  print(varname)
  if(vartype=="select_multiple"){return(triangulate_combine_select_multiple)}
  if(vartype=="select_one"){return(triangulate_mode)}
  if(vartype=="numeric"){return(triangulate_mean)}
}





