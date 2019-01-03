
expand_composite_indicators_to_set_intersections<-function(data,compnames){
  newvarnames<-lapply(1:length(compnames),function(x){
    combn(compnames,x) %>% apply(2,paste,collapse="&")
  }) %>% unlist
  data <- lapply(data[,compnames],as.logical)
  attach(data)
  setintersections <- lapply(newvarnames,function(x){
    eval(parse(text = x))
  })
  detach(data)
  setintersections<-as.data.frame(setintersections)
  names(setintersections)<-newvarnames
  # for msna tool, you might want to use a non-special-character placeholder for "&":
  # names(setintersections)<-gsub("&","._.a.n.d._.",newvarnames)
  return(setintersections)

}

set_intersection_plot<-function(set_percentages){
  set_percentages<-set_percentages*100 %>% round
  upset(fromExpression(set_percentages),
        order.by = "freq", nintersects = 10, nsets = 7,
        mainbar.y.label = "% in need per combination of sectors"
        , mainbar.y.max = 15
  )
}
