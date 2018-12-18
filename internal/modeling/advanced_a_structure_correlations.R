```{r setup0, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
source("./internal/R/120 - dependencies.R")
.log<-list();.logfile<-"./output/log/log.txt";.clearlog()
source("./internal/R/130 - load_input.R")
source("./internal/R/140 - composite_indicators.R")

#Advcanced Analytics
suppressPackageStartupMessages(library(installr))
installr:updateR()
require(survey)
library(effects)
library(sjPlot)  
library(outliers)
library(magrittr)
  
hist(data$tot_income, breaks= 80)data$tot_income

# data$consumption_per_capita_per_month <- data$consumption_per_capita_per_day * 30.43
data$tot_income[is.na(data$tot_income)] <- 0 
data$log_tot_income <- log10(data$tot_income)

# there are many missing values in the tot_income calculation, representing those HH with 0 income
data$log_tot_income[is.na(data$log_tot_income)] <- 0
data$log_tot_income[data$log_tot_income < 0] <- 0

#sanitise
#remove NA's
data <- data[!is.na(data[["log_tot_income"]]),]
data$weight_nat <- as.numeric(data$weight_nat)
data <- data[!(data[["weight_nat"]] == 0),]

#apply weights and create design object (must happen after recoding)

      design <- svydesign(~0, data = data, weights = data$weight_nat)

##### 2.0 Inspecting dependent variable
#### do a histogram 
hist(data$log_tot_income, breaks= 80) ### log distribution

##### 2.2 GLM 
summary(lm(consumption_per_capita_per_month ~ large_hh, data = data))


Model_wash <- svyglm(formula = wash_pin ~ log_tot_income, design, family=stats::gaussian())
Model_wash_ind <- svyglm(formula = wash_index ~ log_tot_income, design, family=stats::gaussian())
Model_prot <- svyglm(formula = protection_pin ~ log_tot_income, design, family=stats::gaussian())
Model_livelihood <- svyglm(formula = livelihood_pin ~ log_tot_income, design, family=stats::poisson())
Model_health <- svyglm(formula = health_pin ~ log_tot_income, design, family=stats::gaussian())


Model_health_why <- svyglm(formula = health_pin ~ mcna_clinic * mcna_hosp + mcna_vaccines1 * mcna_chronic1
                         , design, family = stats::gaussian())
Clinics_or_money <- svyglm(formula = log_tot_income ~ mcna_clinic + mcna_hosp + clusterid
                            , design, family = stats::gaussian())

health_income_hospital <- svyglm(formula = log_tot_income ~ mcna_hosp
                            , design, family = stats::gaussian())
hospitals_location <- svyglm(formula = mcna_hosp ~  district
                                     , design, family = stats::gaussian())
health_pin_location <- svyglm(formula = health_pin ~  log_tot_income * mcna_hosp
                              , design, family = stats::gaussian())

summary(health_income_hospital)
summary(health_pin_location)
summary(hospitals_location)

Adhamia Chamchamal Dahuk Darbandihkan Diwaniya 
Haditha Halabja Hamza Hashimiya Kalar Kerbala Khalis Koisnjaq

data$clusterid <- as.character(data$clusterid)

health_geography  <- svyglm(formula = health_index ~ mcna_clinic * clusterid 
                            #+ mcna_vaccines1 * mcna_chronic1
                            , design, family = stats::gaussian())


summary(Model_wash)
summary(Clinics_or_money)
summary(Clinics_and_money_location)
summary(Model_wash_ind)
summary(Model_prot)
summary(Model_livelihood)
summary(Model_health)

summary(health_geography)
summary(Model2)


##### 2.3 Residuals

plot(Model1$y, Model1$residuals)
plot(Model1a$y, Model1a$residuals)
plot(Model2$y, Model2$residuals)

##### 2.4 Testing models against each other AIC

###extract aic from all models 

##### 3.1 Mapping from funtion outputs to data table outputs 

#####