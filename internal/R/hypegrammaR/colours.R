
# INDIVIDUAL COLORS

#' Reach brand reds
#'
#' @param
#' @param
#' @param
#' @return
#' @seealso \code{\link{function_name}}
#' @export
#' @examples
#'
#'
#'
reach_style_color_red<-function(lightness=1){
  if      (lightness==1){rgb(238/255,88/255,89/255)}
  else if (lightness==2){rgb(238/255,88/255,89/255,0.5)}
  else if (lightness==3){rgb(238/255,88/255,89/255,0.3)}
  
}

#' Reach brand dark greys
#'
#' @param
#' @param
#' @param
#' @return
#' @seealso \code{\link{function_name}}
#' @export
#' @examples
#'
#'
#'
reach_style_color_darkgrey<-function(lightness=1){
  if      (lightness==1){rgb(88/255,88/255,90/255)}
  else if (lightness==2){rgb(88/255,88/255,90/255,0.5)}
  else if (lightness==3){rgb(88/255,88/255,90/255,0.3)}
  
}

#' reach brand light greys
#'
#' @param
#' @param
#' @param
#' @return
#' @seealso \code{\link{function_name}}
#' @export
#' @examples
#'
#'
#'
reach_style_color_lightgrey<-function(lightness=1){
  if      (lightness==1){rgb(209/255,211/255,212/255)}
  else if (lightness==2){rgb(209/255,211/255,212/255,0.5)}
  else if (lightness==3){rgb(209/255,211/255,212/255,0.3)}
  
}

#' reach brand beiges
#'
#' @param
#' @param
#' @param
#' @return
#' @seealso \code{\link{function_name}}
#' @export
#' @examples
#'
#'
#'
reach_style_color_beige<-function(lightness=1){
  if      (lightness==1){rgb(210/255,203/255,184/255)}
  else if (lightness==2){rgb(210/255,203/255,184/255,0.5)}
  else if (lightness==3){rgb(210/255,203/255,184/255,0.3)}
  
}

# COLOUR TRIPLES
#' Reach brand reds triples
#'
#'
#' @param
#' @param
#' @param
#' @return
#' @seealso \code{\link{function_name}}
#' @export
#' @examples
#'
#'
#'
reach_style_color_reds<-function(){
  vapply(1:3,FUN.VALUE = vector(mode = 'character',length = 1),reach_style_color_red)
}

# COLOUR TRIPLES
#' Reach brand dark grey triples
#'
#'
#' @param
#' @param
#' @param
#' @return
#' @seealso \code{\link{function_name}}
#' @export
#' @examples
#'
#'
#'
reach_style_color_darkgreys<-function(){
  vapply(1:3,FUN.VALUE = vector(mode = 'character',length = 1),reach_style_color_darkgrey)
}

# COLOUR TRIPLES
#' Reach brand light greys triples
#'
#'
#' @param
#' @param
#' @param
#' @return
#' @seealso \code{\link{function_name}}
#' @export
#' @examples
#'
#'
#'
reach_style_color_lightgreys<-function(){
  vapply(1:3,FUN.VALUE = vector(mode = 'character',length = 1),reach_style_color_lightgrey)
}

# COLOUR TRIPLES
#' Reach brand beige triples
#'
#'
#' @param
#' @param
#' @param
#' @return
#' @seealso \code{\link{function_name}}
#' @export
#' @examples
#'
#'
#'
reach_style_color_beiges<-function(){
  vapply(1:3,FUN.VALUE = vector(mode = 'character',length = 1),reach_style_color_beige)
}

reach_style_color_rainbow<-function(n){
  cols<-c(rev(reach_style_color_darkgreys()[1]),(reach_style_color_reds()[3]),rev(reach_style_color_beiges()[1]))  
  # cols<-rep(cols,ceiling(n/12))[1:3]
  colorRampPalette(cols)(n)
}


# GGPLOT GRADIENTS
scale_fill_reach <- function(color=NULL,name="",...){
  if(is.null(color)){
    structure(list(
      scale_fill_manual(values= reach_style_color_rainbow(22),name=name,...)
    ))
  }else{
    structure(list(
      scale_fill_manual(values= get(paste0('reach_style_color_',color,'s'))(),name=name,...)
      
    ))}
}

scale_color_discrete_reach <- function(color='red'){
  
  structure(list(
    scale_color_manual(values= get(paste0('reach_style_color_',color,'s'))())
  ))
}

scale_color_continuous_reachn <- function(color='red'){
  
  structure(list(
    scale_color_gradientn(colours = get(paste0('reach_style_color_',color,'s'))())
  ))
}



.show_colors<-function(cols){
  
  plot(1:length(cols),1:length(cols),cex=20,pch=20,col=cols,axes=FALSE)
}


