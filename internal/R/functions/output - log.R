

.write_to_log<-function(x){
  .log<-c(.log,x)
  if(!file.exists(.logfile)){sink(.logfile)}else{
    sink(.logfile,append = T)    
  }

  cat("\n\n")
  cat(x)
  sink()
}

.clearlog<-function(){
  sink(.logfile,append = F)
  cat("")
  sink()
}



logmessage<-function(message){
message(message)
.write_to_log(message)
}



