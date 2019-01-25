new_data <- sapply(data, function(x){
  str_replace_all(x, "_", "")}) %>% as.data.frame

write.csv(new_data, "./internal/input_files/data1.csv")
choices <- questionnaire$choices
choices$name <- str_replace_all(choices$name, "_", "")
write.csv(choices, "./internal/input_files/choices1.csv")
questions <- read.csv("internal/input_files/kobo questions.csv")
questions$constraint <- str_replace_all(questions$constraint, "_", "")
questions$relevant <- str_replace_all(questions$relevant, "_", "")
write.csv(questions, "./internal/input_files/kobo questions.csv")

stratification_sf$sampling.frame$stratum <- str_replace_all(stratification_sf$sampling.frame$stratum, "_", "")
write.csv(stratification_sf$sampling.frame, "internal/input_files/stratification_samplingframe.csv")

