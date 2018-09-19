

check_select_multiple_choice_columns<-function(data){
  if(is_questionnaire_loaded()){
      good<-lapply(names(data),function(varname){
          if(!question_is_select_multiple(varname)){return(TRUE)}
        tryCatch({choices_for_select_multiple(varname,data);return(TRUE)},error={return(FALSE)})
      }) %>% unlist
      names(good)<-names(data)
      return(good)
        }else{stop("you must first successfully run load_questionnaire() before select_multiple choice columns can be checked.")}
    }
