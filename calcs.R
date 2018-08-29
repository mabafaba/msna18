setwd("../")
getwd()
dataz <- read.csv(file = "AHTRA_Round2_Analysis_REVISED AGG + DISTRICTS.csv", stringsAsFactors = F)



shelter <- dataz$Basic.Data.In.which.district.is.the.Key.Informant.located. %>% table(.,dataz$Humanitarian.Assistance.If.yes..what.type.of.assistance.has.your.community.received..Shelter, 
          useNA = "no")
shelter<- prop.table(shelter, 1)[,2]


food <- dataz$Basic.Data.In.which.district.is.the.Key.Informant.located. %>% table(.,dataz$Humanitarian.Assistance.If.yes..what.type.of.assistance.has.your.community.received..Food, 
                                                                                      useNA = "no")
food <- prop.table(food, 1)[,2]

health <- dataz$Basic.Data.In.which.district.is.the.Key.Informant.located. %>% table(.,dataz$Humanitarian.Assistance.If.yes..what.type.of.assistance.has.your.community.received..Heallth.care, 
                                                                                   useNA = "no")
health <- prop.table(health, 1)[,2]

water <- dataz$Basic.Data.In.which.district.is.the.Key.Informant.located. %>% table(.,dataz$Humanitarian.Assistance.If.yes..what.type.of.assistance.has.your.community.received..Drinking.water, 
                                                                                     useNA = "no")
water <- prop.table(water, 1)[,2]

hyg <- dataz$Basic.Data.In.which.district.is.the.Key.Informant.located. %>% table(.,dataz$Humanitarian.Assistance.If.yes..what.type.of.assistance.has.your.community.received..Hygiene.training...kits, 
                                                                                    useNA = "no")
hygiene.kit <- prop.table(hyg, 1)[,2]

cash <- dataz$Basic.Data.In.which.district.is.the.Key.Informant.located. %>% table(.,dataz$Humanitarian.Assistance.If.yes..what.type.of.assistance.has.your.community.received..Cash.assistance, 
                                                                                  useNA = "no")
cash <- prop.table(cash, 1)[,2]

edu <- dataz$Basic.Data.In.which.district.is.the.Key.Informant.located. %>% table(.,dataz$Humanitarian.Assistance.If.yes..what.type.of.assistance.has.your.community.received..Education.for.children.under.18, 
                                                                                  useNA = "no")
education <- prop.table(edu, 1)[,2]

psyc <- dataz$Basic.Data.In.which.district.is.the.Key.Informant.located. %>% table(.,dataz$Humanitarian.Assistance.If.yes..what.type.of.assistance.has.your.community.received..Psychological.support, 
                                                                                  useNA = "no")
psychosocial <- prop.table(psyc, 1)[,2]

oth <- dataz$Basic.Data.In.which.district.is.the.Key.Informant.located. %>% table(.,dataz$Humanitarian.Assistance.If.yes..what.type.of.assistance.has.your.community.received..Other..Specify., 
                                                                                  useNA = "no")
other <- prop.table(oth, 1)[,2]

humanitarian.assistance <- cbind(shelter, water, health, hygiene.kit, cash, education, psychosocial, other)
write.csv(humanitarian.assistance, "./Lea.csv")
