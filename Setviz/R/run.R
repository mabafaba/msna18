setwd(dirname(rstudioapi::getActiveDocumentContext()$path));
getwd()
setwd("./../..")

# which ones should be intersected?
composite_indicator_names<-c("MCNA_FoodSec1", "health_score1", "wash3_score1",
                             "MCNA_education_score1", "protection_scoreSFHH2", "shelter_score1",
                             "live_score1")

#
