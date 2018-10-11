#Advcanced Analytics
suppressPackageStartupMessages(library(installr))
installr:updateR()
require(survey)
library(effects)
library(sjPlot)  
library(outliers)
library(magrittr)

##### Starting with glms 

##### 1.0 Mapping from data inputs to function inputs

  vars <- parameters$variables
  
##### 1.1 Sanitation 


##### 1.2Recoding 
  #1.2.1 Depdendent var
# data$consumption_per_capita_per_day %>% table  
data$consumption_per_capita_per_month <- data$consumption_per_capita_per_day * 30.43
data$log_consumption_per_capita_per_month <- log10(data$consumption_per_capita_per_month)
data$log_consumption_per_capita_per_month[data$log_consumption_per_capita_per_month < 0] <- NA

dependent.var <- parameters$dependent.var

  #1.2.2 Independent 
data$large_hh <- (data$hh_size > 6) %>% ordered 
data$dependency_ratio_greater_1 <- (data$dep_ratio_.1 == "yes")%>% ordered
data$fcs_acceptable <- (data$fcs_category == "acceptable") %>% ordered
data$fcs_borderline <- (data$fcs_category == "borderline") %>% ordered
data$std_dwelling <- (data$standard_dwelling == "yes") %>% ordered
data$hh_holding_debt <- (data$hh_holding_debt == "yes") %>% ordered
data$pis_regular.income <- (data$pis_regular.income == "yes") %>% ordered
data$ncs_debt <- (data$ncs_debt == "yes") %>% ordered



#sanitise
#remove NA's
data <- data[!is.na(data[[dependent.var]]),]
data <- data[!(data[[parameters$weight]] == 0),]


#apply weights and create design object (must happen after recoding)
if(exists("parameters$weight")){
  design <- map_to_design(data)}else{
    design <- svydesign(~1, variables = data, weights = data[[parameters$weight]])}

#making a dataframe with the variables 
cols<- names(data) %in% vars
data_for_model <- data[,cols]
data[,cols] <- as.data.frame(lapply(data_for_model, as.factor))

##### 2.0 Inspecting dependent variable
#### do a histogram 
hist(data$log_consumption_per_capita_per_month, breaks= 80) ### log distribution
# hist(log(data$total_quantity_water), breaks = 100) # <- normal distribution

hist(data$log_consumption_per_capita_per_month, breaks = 30) # <- linear
data$log_consumption_per_capita_per_month %>% table

# data$quality_water[data$quality_water %in% c(98, 99)] <- NA
# data$log_hh_water_consumption <- log(data$hh_water_consumption)
# data$log_hh_water_consumption[data$log_hh_water_consumption < 0] <- 0

##### 2.1 Correlation matrix of potential predictors (independent variable)
data_for_model[] <- lapply(data_for_model,as.integer)
sjPlot::sjp.corr(data_for_model)

##### 2.2 GLM 
summary(lm(consumption_per_capita_per_month ~ large_hh, data = data))

Model1 <- svyglm(formula = consumption_per_capita_per_month ~ large_hh + dependency_ratio_greater_1 + fcs_borderline + hh_holding_debt, design, family=stats::gaussian())
Model1a <- svyglm(formula = log_hh_water_consumption ~ quality_water, design, family=stats::gaussian())
Model2 <- svyglm(formula = as.numeric(quality_water) ~ hh_water_consumption, design, family=stats::poisson())

summary(Model1)
summary(Model2)
##### 2.3 Residuals

plot(Model1$y, Model1$residuals)
plot(Model1a$y, Model1a$residuals)
plot(Model2$y, Model2$residuals)

##### 2.4 Testing models against each other AIC

###extract aic from all models 

##### 3.1 Mapping from funtion outputs to data table outputs 

#####