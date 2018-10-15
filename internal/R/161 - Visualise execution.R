# # dependencies:
# install.packages("UpSetR")
require("dplyr")
require("UpSetR")
require("magrittr")

# # which ones should be intersected?
composite_indicator_names<-c("education_PIN", "health_PIN", "protection_PIN", "food_PIN", "livelihood_PIN", "NFI_PIN", "WASH_PIN") %>% to_alphanumeric_lowercase
compinds <- data[,composite_indicator_names] 
compinds[is.na(compinds)] <- 0
# # assuming they are coercible to logical (e.g. 0's and  1's)
# # you can create TRUE/FALSE columns for call combinations with this:
intersected_composites<- expand_composite_indicators_to_set_intersections(data,composite_indicator_names)
intersected_composites[is.na(intersected_composites)] <- 0 
intersected_composites <- lapply(intersected_composites, as.numeric)

intersected_composites <- as.data.frame(intersected_composites)
# ### get aggregated results...:
# ### this should be done with the msna tool in the real example to make sure everything is weighted correctly etc.:
# ### If you have used load_questionnaire() you could also do this with the "reachR" package (Eliora knows)
# ### now doing this quick and cheap for the example:
aggregated.results<- apply(intersected_composites, MARGIN = 2, mean)

# # aggregated.resu
# lts must be a vector with the names given as set1, set2, set1&set2 ... etc (as given by expand_composite_indicators_to_set_intersections()):
# # then we can do:
set_intersection_plot(intersected_composites, nsets = 7, nintersects = 12)
upset(intersected_composites, order.by = "freq",mainbar.y.max = 10, nsets = 7, nintersects = 12)
