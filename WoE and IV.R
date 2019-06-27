
rm(list=ls())
library(data.table)
library(ggplot2)
library(dplyr)
library(stringr)
library(RANN)
library(h2o)
library(caret)
library(gridExtra)
library(corrplot)
library(tidyr)
library(cvAUC)
library(plotly)
library(pwr)
library(reshape)

#Lets begin by reading in and examining our data

## Importing the Data
# Setup how the classes will be read in
class <- c(	"numeric",	"numeric", "numeric",	"character",	"character",	"numeric",	"numeric",
           "numeric", "numeric",	"numeric",	"numeric",	"numeric",	"character",	"character",	"numeric",
           "date",	"numeric", "numeric",	"character",	"numeric",	"numeric",	"character")


path <- c("/Users/mustafaozturk/Desktop/eBay Case/DataSet/cars_dataset_may2019.csv")

# Read in and examine the data
cars <- data.table::fread(path, colClasses = class)


#Converting NA to 0
cars$telclicks <- ifelse(is.na(cars$telclicks), "0", cars$telclicks)
cars$bids <- ifelse(is.na(cars$bids), "0", cars$bids)
cars$webclicks <- ifelse(is.na(cars$webclicks), "0", cars$webclicks)


#Changing NA, None and ? to .
cars[cars == "None"] <- NA
cars[cars == "?"] <- NA

cars$energielabel       <- as.factor(cars$energielabel)

carsAB <- cars %>% filter(group == "A" | group == "B")
# WOE and  IF implementation in R

## We will use the scorecard package and an example dataset to investigate the concepts of WOE and IV and test its implementation in R


data <- carsAB %>%
                as_tibble()

#replace '.' in variable names not compatible with f_train_lasso
vars = names(data) %>%
  str_replace_all( '\\.', '_')

names(data) <- vars

data$telclicks <- as.numeric(data$telclicks)
data$bids <- as.numeric(data$bids)
data$webclicks <- as.numeric(data$webclicks)
data$n_asq <- as.numeric(data$n_asq)


#Create dummy variables

data <- data %>% mutate(age = as.numeric(format(Sys.Date(), "%Y")) -
                          as.integer(data$bouwjaar),
                        annual_emissions = as.numeric(emissie)/age,
                        annual_kms = kmstand / age)
# create an age grouping
data <- data %>% mutate(ageGroup = ifelse(age<= 3, "(<=3)", 
                                          ifelse(3 < age & age <= 6, "(4-6)",
                                                 ifelse(5 < age & age <= 10, "(7-10)",
                                                        ifelse(10 < age & age <= 15, "(11-15)",
                                                               ifelse(15 < age & age <= 20, "(16-20)", "(20+)"))))))
data$ageGroup <- as.factor(data$ageGroup)


# convert response factor variable to dummy variable
data <- data %>% mutate(total_clicks = (telclicks + webclicks + n_asq +bids))
# create the response variable (label)
data$clicked       <- ifelse(data$total_clicks > 0, 1, 0)
data$clicked       <- as.factor(data$clicked)
data$kleur       <- as.factor(data$kleur)
data$kleur2 <- ifelse(data$kleur=="Zilver of Grijs","Zilver of Grijs",
                    ifelse(data$kleur=="Zwart","Zwart",
                           ifelse(data$kleur=="Blauw","Blauw",
                             ifelse(data$kleur=="Wit","Wit",
                                    ifelse(data$kleur=="Rood","Rood",
                                           ifelse(is.na(data$carrosserie),"NA's","Other"))))))

data$carrosserie       <- as.factor(data$carrosserie)

data$carrosserie2 <- ifelse(data$carrosserie=="Hatchback (3/5-deurs)","Hatchback (3/5-deurs)",
                      ifelse(data$carrosserie=="MPV","MPV",
                             ifelse(data$carrosserie=="Sedan (2/4-deurs)","Sedan (2/4-deurs)",
                                    ifelse(data$carrosserie=="Terreinwagen","Terreinwagen",
                                           ifelse(data$carrosserie=="Stationwagon","Stationwagon",
                                                  ifelse(is.na(data$carrosserie),"NA's","Other"))))))
data$energielabel2 <- ifelse(data$energielabel=="A","A",
                            ifelse(data$energielabel=="B","B",
                                   ifelse(data$energielabel=="C","C",
                                          ifelse(data$energielabel=="D","D",
                                                 ifelse(data$energielabel=="E","E",
                                                        ifelse(is.na(data$energielabel),"NA's","Other"))))))
data$brand2 <- ifelse(data$brand=="VOLKSWAGEN","VOLKSWAGEN",
                             ifelse(data$brand=="PEUGEOT","PEUGEOT",
                                    ifelse(data$brand=="RENAULT","RENAULT",
                                           ifelse(data$brand=="OPEL","OPEL",
                                                  ifelse(data$brand=="FORD","FORD",
                                                         ifelse(data$brand=="BMW","BMW","Other"))))))

data$aantalstoelen2 <- ifelse(data$aantalstoelen=="2","2",
                             ifelse(data$aantalstoelen=="3","3",
                                    ifelse(data$aantalstoelen=="4","4",
                                           ifelse(data$aantalstoelen=="5","5",
                                                  ifelse(data$aantalstoelen=="7","7",
                                                         ifelse(is.na(data$aantalstoelen),"NA's","Other"))))))
data$emissie2 <- ifelse(data$emissie=="0","0",
                              ifelse(data$emissie=="139","139",
                                     ifelse(data$emissie=="99","99",
                                            ifelse(data$emissie=="119","119",
                                                   ifelse(data$emissie=="109","109",
                                                          ifelse(is.na(data$emissie),"NA's","Other"))))))
data$model2 <- ifelse(data$model=="Golf","Golf",
                        ifelse(data$model=="Polo","Polo",
                               ifelse(data$model=="Astra","Astra",
                                      ifelse(data$model=="Focus","Focus",
                                             ifelse(data$model=="Corsa","Corsa",
                                                    ifelse(is.na(data$model),"NA's","Other"))))))
data$price2 <- ifelse(data$price<10000,"[-inf,10.000)",
                      ifelse(data$price<50000,"[10.000,50.000)",
                             ifelse(data$price<90000,"[50.000,90.000)",
                                    ifelse(is.na(data$price),"NA's","[90.000,inf)"))))

data$kleur2       <- as.factor(data$kleur2)
data$carrosserie2       <- as.factor(data$carrosserie2)
data$energielabel2       <- as.factor(data$energielabel2)
data$aantalstoelen2       <- as.factor(data$aantalstoelen2)
data$emissie2       <- as.factor(data$emissie2)
data$model2       <- as.factor(data$model2)
data$brand2       <- as.factor(data$brand2)

data$aantaldeuren       <- as.factor(data$aantaldeuren)
data$emissie       <- as.factor(data$emissie)
data$brand       <- as.factor(data$brand)
data$l2       <- as.factor(data$l2)
data$model       <- as.factor(data$model)
data$aantalstoelen       <- as.factor(data$aantalstoelen)
summary(data)


#############################
######## A/B TESTING ########
#############################


data%>%group_by(group)%>%summarise(cnt=n())%>%group_by(clicked)%>%summarise(cnt2=n())
data


clickgroup <- group_by(data, group, clicked)
groupclick <- summarize(clickgroup, clicks = n())
groupclick


final<-cast(groupclick,group~clicked)
final


#Lift calculation 
lift <- (final[final$group=='B',3]/(final[final$group=='B',3]+final[final$group=='B',2]))/(final[final$group=='A',3]/(final[final$group=='A',3]+final[final$group=='A',2]))


#Price Groups
priceclickgroup <- group_by(data, group,price2, clicked)
pricegroupclick <- summarize(priceclickgroup, clicks = n())
print(pricegroupclick)


pricefinal<-cast(pricegroupclick,group+price2~clicked)
print(pricefinal)



#Price [-inf,10.000) 
liftprice1 <- (pricefinal[pricefinal$group=='A'&pricefinal$price2 =='[-inf,10.000)',4]/(pricefinal[pricefinal$group=='A'&pricefinal$price2 =='[-inf,10.000)',3 ]+pricefinal[pricefinal$group=='A'&pricefinal$price2 =='[-inf,10.000)',4 ]))/(pricefinal[pricefinal$group=='B'&pricefinal$price2 =='[-inf,10.000)',4 ]/(pricefinal[pricefinal$group=='B'&pricefinal$price2 =='[-inf,10.000)',3 ]+pricefinal[pricefinal$group=='B'&pricefinal$price2 =='[-inf,10.000)',4 ]))

print(liftprice1)
#Price [10.000,50.000)
liftprice2 <- (pricefinal[pricefinal$group=='A'&pricefinal$price2 =='[10.000,50.000)',4]/(pricefinal[pricefinal$group=='A'&pricefinal$price2 =='[10.000,50.000)',3 ]+pricefinal[pricefinal$group=='A'&pricefinal$price2 =='[10.000,50.000)',4 ]))/(pricefinal[pricefinal$group=='B'&pricefinal$price2 =='[10.000,50.000)',4 ]/(pricefinal[pricefinal$group=='B'&pricefinal$price2 =='[10.000,50.000)',3 ]+pricefinal[pricefinal$group=='B'&pricefinal$price2 =='[10.000,50.000)',4 ]))

print(liftprice2)
#Price [50.000,90.000)
liftprice3 <- (pricefinal[pricefinal$group=='A'&pricefinal$price2 =='[50.000,90.000)',4]/(pricefinal[pricefinal$group=='A'&pricefinal$price2 =='[50.000,90.000)',3 ]+pricefinal[pricefinal$group=='A'&pricefinal$price2 =='[50.000,90.000)',4 ]))/(pricefinal[pricefinal$group=='B'&pricefinal$price2 =='[50.000,90.000)',4 ]/(pricefinal[pricefinal$group=='B'&pricefinal$price2 =='[50.000,90.000)',3 ]+pricefinal[pricefinal$group=='B'&pricefinal$price2 =='[50.000,90.000)',4 ]))

print(liftprice3)
#Price [90.000,inf)
liftprice4 <- (pricefinal[pricefinal$group=='A'&pricefinal$price2 =='[90.000,inf)',4]/(pricefinal[pricefinal$group=='A'&pricefinal$price2 =='[90.000,inf)',3 ]+pricefinal[pricefinal$group=='A'&pricefinal$price2 =='[90.000,inf)',4 ]))/(pricefinal[pricefinal$group=='B'&pricefinal$price2 =='[90.000,inf)',4 ]/(pricefinal[pricefinal$group=='B'&pricefinal$price2 =='[90.000,inf)',3 ]+pricefinal[pricefinal$group=='B'&pricefinal$price2 =='[90.000,inf)',4 ]))

print(liftprice4)

resultPrice <- rbind(liftprice1,liftprice2,liftprice3,liftprice4)

#############################################

#Age Groups
ageclickgroup <- group_by(data, group,ageGroup, clicked)
agegroupclick <- summarize(ageclickgroup, clicks = n())
agegroupclick


agefinal<-cast(agegroupclick,group+ageGroup~clicked)
agefinal



#Age <=3 
liftAge3 <- (agefinal[agefinal$group=='B'&agefinal$ageGroup =='(<=3)',4]/(agefinal[agefinal$group=='B'&agefinal$ageGroup =='(<=3)',3 ]+agefinal[agefinal$group=='B'&agefinal$ageGroup =='(<=3)',4 ]))/(agefinal[agefinal$group=='A'&agefinal$ageGroup =='(<=3)',4 ]/(agefinal[agefinal$group=='A'&agefinal$ageGroup =='(<=3)',3 ]+agefinal[agefinal$group=='A'&agefinal$ageGroup =='(<=3)',4 ]))

print(liftAge3)

#Age 4-6
liftAge46 <- (agefinal[agefinal$group=='A'&agefinal$ageGroup =='(4-6)',4]/(agefinal[agefinal$group=='A'&agefinal$ageGroup =='(4-6)',3 ]+agefinal[agefinal$group=='A'&agefinal$ageGroup =='(4-6)',4 ]))/(agefinal[agefinal$group=='B'&agefinal$ageGroup =='(4-6)',4 ]/(agefinal[agefinal$group=='B'&agefinal$ageGroup =='(4-6)',3 ]+agefinal[agefinal$group=='B'&agefinal$ageGroup =='(4-6)',4 ]))

print(liftAge46)

#Age 7-10
liftAge710 <- (agefinal[agefinal$group=='A'&agefinal$ageGroup =='(7-10)',4]/(agefinal[agefinal$group=='A'&agefinal$ageGroup =='(7-10)',3 ]+agefinal[agefinal$group=='A'&agefinal$ageGroup =='(7-10)',4 ]))/(agefinal[agefinal$group=='B'&agefinal$ageGroup =='(7-10)',4 ]/(agefinal[agefinal$group=='B'&agefinal$ageGroup =='(7-10)',3 ]+agefinal[agefinal$group=='B'&agefinal$ageGroup =='(7-10)',4 ]))

print(liftAge710)

#Age 11-15 
liftAge1115 <- (agefinal[agefinal$group=='A'&agefinal$ageGroup =='(11-15)',4]/(agefinal[agefinal$group=='A'&agefinal$ageGroup =='(11-15)',3 ]+agefinal[agefinal$group=='A'&agefinal$ageGroup =='(11-15)',4 ]))/(agefinal[agefinal$group=='B'&agefinal$ageGroup =='(11-15)',4 ]/(agefinal[agefinal$group=='B'&agefinal$ageGroup =='(11-15)',3 ]+agefinal[agefinal$group=='B'&agefinal$ageGroup =='(11-15)',4 ]))

print(liftAge1115)

#Age 16-20
liftAge1620 <- (agefinal[agefinal$group=='A'&agefinal$ageGroup =='(16-20)',4]/(agefinal[agefinal$group=='A'&agefinal$ageGroup =='(16-20)',3 ]+agefinal[agefinal$group=='A'&agefinal$ageGroup =='(16-20)',4 ]))/(agefinal[agefinal$group=='B'&agefinal$ageGroup =='(16-20)',4 ]/(agefinal[agefinal$group=='B'&agefinal$ageGroup =='(16-20)',3 ]+agefinal[agefinal$group=='B'&agefinal$ageGroup =='(16-20)',4 ]))

print(liftAge1620)

#Age 20+
liftAge20<- (agefinal[agefinal$group=='A'&agefinal$ageGroup =='(20+)',4]/(agefinal[agefinal$group=='A'&agefinal$ageGroup =='(20+)',3 ]+agefinal[agefinal$group=='A'&agefinal$ageGroup =='(20+)',4 ]))/(agefinal[agefinal$group=='B'&agefinal$ageGroup =='(20+)',4 ]/(agefinal[agefinal$group=='B'&agefinal$ageGroup =='(20+)',3 ]+agefinal[agefinal$group=='B'&agefinal$ageGroup =='(20+)',4 ]))

print(liftAge20)


resultAge <- rbind(liftAge3,liftAge46,liftAge710,liftAge1115,liftAge1620,liftAge20)

#############################
########  SCORECARD  ########
#############################

library(scorecard)
?scorecard

names(data)
data <- subset( data, select = -c(telclicks, bids,n_asq, l2,webclicks,emissie,model,brand,
                                  total_clicks,kleur,carrosserie,energielabel,aantalstoelen) ) 

summary(data)
iv = iv(data, y = 'clicked') %>%
  as_tibble() %>%
  mutate( info_value = round(info_value, 3) ) %>%
  arrange( desc(info_value) )

iv %>%
  knitr::kable()
#Weight of Evidence Binning



bins = woebinMus(data, y = 'clicked')
woebin()

bins$price %>%
  knitr::kable()
woebin_plot(bins$group)
woebin_plot(bins$price)
woebin_plot(bins$bouwjaar)
woebin_plot(bins$model2)
woebin_plot(bins$emissie)
woebin_plot(bins$kmstand)
woebin_plot(bins$days_live)
woebin_plot(bins$photo_cnt)
woebin_plot(bins$energielabel)
woebin_plot(bins$annual_emissions)
woebin_plot(bins$annual_kms)
woebin_plot(bins$vermogen)
woebin_plot(bins$brand)
woebin_plot(bins$aantaldeuren)
woebin_plot(bins$carrosserie)
woebin_plot(bins$kleur)
woebin_plot(bins$age)



#Devam edip bakılmalı



#WoE binning application


data_woe = woebin_ply( data, bins ) %>%
  as_tibble()

