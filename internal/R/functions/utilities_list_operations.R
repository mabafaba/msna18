#' flattens a list by one level on specified indices; e.g.:
#  list(list(1,2),3,4,list(5,6),list(7,8)) %>% flatten_list_items(c(1,4))
# makes:
# list(1,2,3,4,5,6,list(7,8)))
flatten_list_items<-function(l,item_indices){
  flatten_list_single_item<-function(l,item_index){
  pre_index<-cbind(1:(item_index-1),NA)
  if(item_index==1){pre_index<-NULL}
  post_index<-cbind((item_index+1):length(l),NA)
  if(item_index==length(l)){post_index<-NULL}
  to_flatten_index<-cbind(item_index,1:length(l[[item_index]]))
  
  indexmatrix<-rbind(pre_index,to_flatten_index,post_index)
  indexmatrix<-lapply(1:nrow(indexmatrix),function(i){return(indexmatrix[i,] %>% as.vector)})
  l2<-lapply(indexmatrix,function(ii){
    if(is.na(ii[2])){
      return((l[[ii[1]]]))
    }else{
      return(l[[ii[1]]][[ii[2]]])
    }
  })
  return(l2)
  }
  for(i in sort(item_indices,decreasing = T)){
    l<-flatten_list_single_item(l,i)
  } 
  l

}
