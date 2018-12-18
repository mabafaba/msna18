library(dplyr)

attach(data)

data$intentions_idp %>% table

data_idp <- data %>% filter(hh_type == "idp")
data_returnee <- data %>% filter(hh_type == "returnee")
data_returnee_idp <- data %>% filter(hh_type %in% c("returnee", "idp"))

idp_intent <- data %>% filter(hh_type == "idp" & !intentions_idp %in% c("wait_to_decide", "remain") & !res_house_land == 1)

data$res_house_land %>% table

idp_intent$future_need <- 1
idp_intent$future_need


no_shelter_need <- recode_equal(data$mcna_shelter_imm, "1", 1) %>% as.vector %>% is.na
idp <- recode_equal(data$hh_type, "idp", 1) %>% as.vector %>% is.na
idp <- !idp
return <- recode_any(data$intentions_idp, c("move_inside_iraq", "move_outside_iraq", "return"), 1) %>% as.vector %>% is.na
return <- !(return)
have_land <- recode_equal(data$res_house_land, "0", 1) %>% as.vector %>% is.na  
have_land <- !have_land

result_og <- rowSums(cbind(no_shelter_need, idp, return))
result <-rowSums(cbind(idp, return))
result <- recode_equal(result, 2, 1)  %>% as.vector %>% is.na
sum(result)
result <- as.vector(!result)
names(result) <- "future_shelter_need"

result %>% table

data$future_shelter_need_all_idps <- result
data$idps_new_movements <- data$future_shelter_need_all_idp

design <- map_to_design(data_idp, weights = data$weight_gvt)

no_need_data <- data %>% filter(mcna_shelter_imm == 0)

data$mcna_shelter_imm %>% table

data$idps_new_movements <- as.character(data$idps_new_movements )

independent.var <- "mcna_shelter_imm"
dependent.var <- "idps_new_movements"
hypothesis.type <-"group_difference"

data_returnee$governorate %>% table

diff <- analyse_indicator(data_idp, dependent.var, independent.var, hypothesis.type, case = "CASE_group_difference_categorical_categorical")
diff$summary.statistic

summ_true_og_aoo <- diff$summary.statistic[c(17:32),]
map_to_file(diff$summary.statistic, "catgeories_moving.csv")
barchart_percent(summ_true_og_aoo, "bar_gvt.jpg")
