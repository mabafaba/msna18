verify_excel_input<-function(){
  verify_excel_input_all_files_exist()
}


verify_excel_input_all_files_exist<-function(){

  files_needed<-c(  "./internal/input_files/analysis plan.csv",
                    "./internal/input_files/choice ranks.csv",
                    "./internal/input_files/cluster_samplingframe.csv",
                    "./internal/input_files/composite_indicators.csv",
                    "./internal/input_files/data.csv",
                    "./internal/input_files/kobo choices.csv",
                    "./internal/input_files/kobo questions.csv",
                    "./internal/input_files/parameters.csv",
                    "./internal/input_files/stratification_samplingframe.csv")
  
  filesexist<-sapply(files_needed,file.exists)
  .write_to_log("which necessary input files exist?")
  suppressWarnings(.write_to_log(kable(filesexist %>% as.matrix)))
  
  if(any(!filesexist)){
    missing_sheets<-files_needed[!filesexist] %>%  strsplit("/") %>% lapply(function(x){x[length(x)] %>% gsub(".csv","",.)}) %>% unlist %>% paste(collapse = "\n")
    stop(paste0(
      "Input information seems to be missing.
      Please open the input xlsx sheets for data and analysis definition and click the update button in the readme sheet of each file.
      Make sure the input xlsm files  contain all sheets from the template with the names unchanged! Not exported sheets:\n",
      missing_sheets))
  }
}
