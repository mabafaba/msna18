





normweights<-function(w,groups=NULL){
  if(is.null(groups)){return(w/sum(w)*length(w))}
  if(!is.null(groups)){
    for(group in groups){
      w[group==groups]<-normweights(w[group==groups])
    }
    return(w)
  }
    
  }



w1<-normweights(c(30,60))
groups_w1<-c('A','B')

w2<-normweights(c(5,10,10))
groups_w2<-(c('A','A','B'))

w1
w2                       

data.frame(group=groups_w1[match(groups_w2,groups_w1)],group2=groups_w2,weight1=w1[match(groups_w2,groups_w1)],weight2=w2)

groups<-names(sw)

cw*sw %>% normweights(groups=groups)
multweights(sw,cw,groups=names(sw))

multweights<-function(w1,w2,groups){
  for(group in groups){
    w[group==groups]<-normweights(w[group==groups])
    
  }
  
  
  

