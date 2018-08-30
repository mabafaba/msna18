.log<-list()
.logfile<-"./output/log/log.txt"

.write_to_log<-function(x){
  .log<-c(.log,x)
  sink(.logfile,append = T)
  cat("\n\n")
  cat(x)
  sink()
}

.clearlog<-function(){
  sink(.logfile,append = F)
  cat("")
  sink()
}


.clearlog()