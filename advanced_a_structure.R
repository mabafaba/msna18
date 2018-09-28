#Advcanced Analytics

##### Starting with glms 

design <- map_to_design(data)

##### 1.1 Sanitation 
  
  
##### 1.1 Mapping from data inputs to function inputs
  
  
##### 2.0 Inspecting dependent variable
#### do a histogram 
hist(data$total_quantity_water, breaks= 100) ### log distribution
hist(log(data$total_quantity_water), breaks = 100) # <- normal distribution
hist(data$wash_scale) # <- Poisson
hist(data$wash_index) # <- binomial 

data$quality_water[data$quality_water %in% c(98, 99)] <- NA
data$log_hh_water_consumption <- log(data$hh_water_consumption)
data$log_hh_water_consumption[data$log_hh_water_consumption < 0] <- 0

data$

##### 2.1 Correlation matrix of potential predictors (independent variable)
  
##### 2.2 GLM 
design <- map_to_design(data)

Model1 <- svyglm(formula = log_hh_water_consumption ~ quality_water + domain, design, family=stats::gaussian())
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