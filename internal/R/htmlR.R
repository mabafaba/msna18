percentlabel<-function(x){
  paste0(round(x*100),'%')
}

easy.html.bars<-function(group,var,width="100%"){
  

bars<-  seq_along(group) %>% lapply(function(x){
    rectangle<-div(x="",
        style=css_styles(
          
          width=percentlabel(var[x]),
          "background-color"="#FF0000",
#           "border-bottom-width"="15px",
#           "border-bottom"="solid",
#           "border-color"="white",
          height="100%")
        )
    
    label<-div(paste(group[x],percentlabel(var[x])),
               style=css_styles(width="100%",height=c("100%"),"text-align"="right",overflow='hidden'))

   
    bar_container<-div(paste(div(label,style="width:40%;display:inline-block;"),
                             div(rectangle,style="width:40%;height:15px;display:inline-block;background-color:#CCCCCC;border-color;white'")),
                       style=css_styles(width=width,border='solid','border-color'='white'))    
    
  }) %>% do.call(paste,.)

barchart<-  div(bars,
      style=paste0("position:relative;"),
      width=width)


}





#########################################################################################
#  BASIC HTML (NO CAT, NO SINK)
#########################################################################################
#########################################################################################

    # STRUCTURAL
    
        tag<-function(tag,x=NULL,...){
          
          open<-(paste0("<",tag,html_attributes(...)," >"))
          if(is.null(x)){
            content=""
            close=""
          }else{
            content=x
            close=paste0("</",tag,">")
          }
          return(paste(open,"\n",content,"\n",close))  
        }
        
    
    
        
        html_attributes<-function(...){
          attributes<- list(...) 
          lapply(names(attributes),function(x){paste0(" ",x,' = "',attributes[[x]],'"')}) %>%
            paste(collapse=" ")
        }
        
        
        
        css_styles<-function(...){
          styles<-list(...)
          lapply(names(styles),function(x){paste0(x,":",styles[[x]],";")}) %>%
            paste(collapse=" ")
        }
        
    

    # COMMON TAGS
          
           div<-function(...){tag("div",...)}
         
                
      
      
          link<-function(text,link,...){
              tag("a",text,href=link,...)
            
          }

                  
        


#########################################################################################
#  WRITING TO FILES
#########################################################################################
#########################################################################################
          
          
          .record<-function(to.file="./output/validation_log.htm"){
            recorder.file<-to.file
            # all functions that need the filename here
            start.speaking<<-function(){
              if(exists("recorder.file")){
                sink(recorder.file,append = T,split = F)
#                 cat("<br>")
              }
            }
            
            
            .rewind<<-function(){
              if(exists("recorder.file")){
                sink(recorder.file,append = F)
                sink()
              }
              .sing()
            }
            .sing()
          }
          
#           .rewind<-function(){
#             warning("don't know what to rewind. call recordto(file) first")
#           }
          
          
          .say<-function(text){
            start.speaking()
            try(generic.speak(text))
            stop.speaking()
            
          }
          
          .shout<-function(text){
            
            start.speaking()
           .break()
            cat("<h3>")
            generic.speak(text)
            cat("</h3>")
            stop.speaking()
            
          }
          
          .scream<-function(text){
            .saytag("br")
            .saytag("br")
            .saytag("br")
            .break()
            
            start.speaking()
            
            cat("<h1>")
            generic.speak(text)
            cat("</h1>")
            stop.speaking()
            
          }
          
          .yell<-function(text){
            start.speaking()
            
            cat("<h4>")
            generic.speak(text)
            cat("</h4>")
            stop.speaking()
            
          }
          stop.speaking<-function(){
#             cat("<br>")
            sink()
          }
          
          
          generic.speak<-function(x){
            if(is.matrix(x) | is.table(x) | is.data.frame(x)){
              kable(x,format="html") %>% print
            } else if(is.list(x)){
              print("islist")
              for(item in 1:length(x)){
                if(length(names(item))>0){
                  shout(names(item)[x])
                }
                .say(item[[x]])
                
              }
            }else{
              cat(x)
            }
          }
          
          .yay<-function(x){
            start.speaking()
            cat("<div style='color:green;'>&#10003;</div>")
            stop.speaking()
          }
          
          
          .break<-function(){
            start.speaking()
            cat("<hr>")
            stop.speaking()
          }
          
          
          .brabble<-function(list_of_matrices){
            
            lapply(names(list_of_matrices),function(x){
              .shout(x)
              .say(list_of_matrices[[x]])
            })
          }

          
          
          .saytag<-function(tag,x=NULL,...){
            .say(tag(tag,x,...))
          }
          
          .saybold<-function(x){
            tag("b",x)
            
          }

#########################################################################################
#  HIGH LEVEL VALIDATION SPECIFIC
#########################################################################################
#########################################################################################         
          
          unfolder<-function(x){
            strsplit(x,"/") %>% unlist %>% last
          }

          
          write.issues<-function(issues,page="issues",title,overview.page="overview"){
            page(overview.page)
            .say('<br>')
            link(text = title,link = paste0('./',page,'.html')) %>%  .say
            .say('<br>')
            page(page)
            .rewind()
            .sing()
            .say(link('overview',paste0('./',overview.page,'.html') %>% unlist %>% last))
            .scream("Potential Data Issues")
            issues$issue_type %>% table %>% .say
            issues %>% .say
              
          }
          
          
          

          write.aggregation.results<-function(results,title,page='aggregation_test',overview.page="overview"){
            page(overview.page)
            .say('<br>')
            link(text = title,link = paste0('./',page,'.html')) %>%  .say
            .say('<br>')
            page(page)
            .rewind()
            .sing()
            .scream(title)
            link.to.overview(page)
            lapply(names(results),function(x){
              .shout(x)
              .say(results[[x]])
              
              if(is.null(results[[x]])){return(NULL)}
              if(length(results[[x]])<2){return(NULL)}
              if(is.na(results[[x]][1])){return(NULL)}
              if(is.numeric(results[[x]])){
                # debug(easy_img.bar)
              # easy_img.bar(group = names(results[[x]]),percent = results[[x]],error_min = 0,error_max = 0 ) %>% resultbox(x) %>% .say
                div(img(width = 10,height = 10,plotfun = function(){barplot(results[[x]],col="black")}),style="width:30%;display:inline-block;") %>% .say
              }
              # .say(easy_img.bar(group = names(results[[x]]),results[[x]],0,0))
            })
            
            
          }


link.to.overview<-function(frompage){
  page(frompage)
  .say('<br>')
  link(text="overview",link='./overview.html')
  .say('<br>')
  }
          
          
#########################################################################################
#  STYLE
#########################################################################################
#########################################################################################          
            
            .sing<-function(){
              start.speaking()
              
              cat("
<style type='text/css'>
                  
                  
                  
                  body,html{
                  font-family:sans-serif;
                  padding:2%;
                  margin:0px;
                  height:100%;
                  width:100%;
                  position:relative;
                  }
                  
                  tr{
                  background-color: #BBBBBB;
                  font-size:0.5em;
                  }
                  
                  h3{
                  font-size:1em;
                  }
                  
                  .result.box{
                  position:relative;
                  width:20%;
                  height:auto;
                  display:inline-block;
                  padding:3%;
                  }
                  
                  img{
                  width:100%;
                  }
                  
                  
                  .result.title{
                  font-size:1em;
                  font-style:bold;
                  font-weight: 500;
                  color:red;
                  width:100%;
                  }
                  .result.graphic{
                  position:relative;
                  width:100%;
                  display:inline-block;
                  }
                  
                  div{
                  margin-left:30px;
                  }
                  </style>")  
              stop.speaking()
              
              
            }
            
            
            


#########################################################################################
# WEB PACKAGE
#########################################################################################

site<-function(path="./output/html"){
  
  .sitepath<<-function(){return(path)}
  .imgpath<<-function(){paste0(path,"/img/")}
  .htmlpath<<-function(){paste0(path,"/html/")}
  .imgpathfromHTML<<-function(){"../img/"}
  
  suppressWarnings(dir.create( .sitepath() ))
  suppressWarnings(dir.create(  .imgpath() ))
  suppressWarnings(dir.create( .htmlpath() ))
  
  }


page<-function(name,wipe=F){
.record(paste0(.htmlpath(),name,".html"))  
if(wipe){.rewind()}
}



img<-function(name=NULL,width,height,plotfun,...){
if(is.null(name)){name<-(runif(1)*1e20) %>% round %>% as.character}
img.target.file<-paste0(.imgpath(),name,'.svg')
img.target.file.path.from.html<-paste0(.imgpathfromHTML() %p0% name %p0% '.svg')


# if(file.exists(img.target.file)){warning(paste("image ",name,"exists already"))}

svg(img.target.file,width,height)
plotfun(...)
dev.off()
html<-tag("img",src=img.target.file.path.from.html)
return(html)

}

'%p0%'<-function(x,y){
  paste0(x,y)
}


