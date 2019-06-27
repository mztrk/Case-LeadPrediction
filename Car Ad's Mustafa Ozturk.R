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
h2o.init(nthreads = -2, max_mem_size = "26G")
sessionInfo()

#Lets begin by reading in and examining our data

## Importing the Data
# Setup how the classes will be read in
class <- c(	"numeric",	"numeric", "numeric",	"character",	"character",	"numeric",	"numeric",
           "numeric", "numeric",	"numeric",	"numeric",	"numeric",	"factor",	"character",	"numeric",
           "date",	"numeric", "numeric",	"character",	"numeric",	"numeric",	"character")


path <- c("/Users/mustafaozturk/Desktop/eBay Case/DataSet/cars_dataset_may2019.csv")

# Read in and examine the data
cars <- data.table::fread(path, colClasses = class)

#Dealing with None and ?




#In some cases like below telclicks, bids and webclicks contains NA. It can be converted to 0
cars[src_ad_id==1063865800,]

#Converting NA to 0
cars$telclicks <- ifelse(is.na(cars$telclicks), "0", cars$telclicks)
cars$bids <- ifelse(is.na(cars$bids), "0", cars$bids)
cars$webclicks <- ifelse(is.na(cars$webclicks), "0", cars$webclicks)

#In some rows of ad_starts contains "." it should change to "/" when I checked the excel file. 
#But it has been taken care by R


table(cars$ad_start_dt)



cars[src_ad_id==1151910234,]

#Changing NA, None and ? to .
cars[cars == "None"] <- NA
cars[cars == "?"] <- NA





# Summary Statistics
str(cars)


# Dimensions
dim(cars)


# Control the test group and check data

table(cars$group)
# 8.613 NULL for Group
cars[cars == "NULL"] <- NA




# Lets explore the data further
# How much data is missing?


MissingValues <- cars %>% summarise_all(funs(sum(is.na(.))/n()))
MissingValues <- gather(MissingValues, key = "feature", value = "MissingPct")
MissingValues %>%
  ggplot(aes(x = reorder(feature, - MissingPct), y = MissingPct)) +
  geom_bar(stat = "identity", fill = "red") +
  coord_flip() + theme_bw()
# A very small percentage of data is NA except l2 and energielabel
# May need to do any KNN/RF imputations for l2 and energielabel


# Create an age variable from date information
# ad start date + days live could be used instead of system time but
# lets the date as of the date of analysis
# transform some features by year using the new age variable
# in order to boost our predictive model power

cars <- cars %>% mutate(age = as.numeric(format(Sys.Date(), "%Y")) -
                          as.integer(cars$bouwjaar),
                        annual_emissions = as.numeric(emissie)/age,
                        annual_kms = kmstand / age)
# create an age grouping
cars <- cars %>% mutate(ageGroup = ifelse(age<= 3, "(<=3)", 
                                          ifelse(3 < age & age <= 6, "(4-6)",
                                                 ifelse(5 < age & age <= 10, "(7-10)",
                                                        ifelse(10 < age & age <= 15, "(11-15)",
                                                               ifelse(15 < age & age <= 20, "(16-20)", "(20+)"))))))
cars$ageGroup <- as.factor(cars$ageGroup)


# Now lets do a visual exploration of our new feature
# View distribution of variable
# Most packed around the 5 Year mark
cars %>% 
  ggplot(aes(x=age))+geom_line(stat="density", color="red", size=1.2)+theme_bw()



# Histogram for a view from another angle by year
ggplot(aes(cars$age), data=cars) +
  geom_histogram(color='white', fill='lightblue') +
  scale_x_continuous(limit=c(0, 35), breaks=seq(0, 35, 2)) +
  labs(x= 'Car Age', y= 'Number of Cars', title= 'Car Age Histogram')


# See if we can unconver anything by segregating by car type
# We have a "NA" factor 
# overiege have a broad spectrum of ages
ggplot(aes(x= carrosserie, y= age), data=cars) +
  geom_boxplot(fill="lightblue", color='black') +
  geom_boxplot(aes(fill = carrosserie)) +
  stat_summary(fun.y = mean, geom="point", size=2) +
  labs(x= 'Vehicle Type', y= 'Age') +
  ggtitle('Age vs. Vehicle Type')

# Examine Car Types
# We have a very high amount of "Hatchbacks" in our dataset
ggplot(cars, aes(x=carrosserie, fill = carrosserie)) + 
  geom_bar() +
  labs(x= 'Vehicle Type', y= 'Number of Cars') +
  ggtitle('Vehicle Type Frequency Diagram')  
# scale_x_(breaks = seq(0, 4.8, 0.5), minor_breaks = seq(0, 4.8, 0.1))

# How long before a car is sold?
# Most cars carry on for the 30+ days
ggplot(data=cars, aes(cars$days_live)) + 
  geom_histogram(breaks=seq(0, 35, by = 5), 
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
  labs(title="Histogram for Days Live") +
  labs(x="Days Live", y="Count")

str(cars)
cars$telclicks <- as.numeric(cars$telclicks)
cars$bids <- as.numeric(cars$bids)
cars$webclicks <- as.numeric(cars$webclicks)
# create total clicks variable
cars <- cars %>% mutate(total_clicks = (telclicks + webclicks + n_asq +bids))
# create the response variable (label)
cars$clicked       <- ifelse(cars$total_clicks > 0, 1, 0)
cars$clicked       <- as.factor(cars$clicked)

cars$TenDaysClick       <- ifelse(cars$days_live < 3, NA, 
                                  ((10*cars$total_clicks)/cars$days_live))


# examine response variable
# as expected, most clicks fall into 0 or 1
ggplot(data=cars, aes(cars$total_clicks)) + 
  geom_histogram(breaks=seq(0, 35, by = 5), 
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
  labs(title="Histogram for Total Clicks") +
  labs(x="Total Clicks", y="Count")

# now that we have our label, lets examine the A/B test results
# create new table with only A/B test results for later analysis
carsAB <- cars %>% filter(group == "A" | group == "B")
carsA <- cars %>% filter(group == "A")
carsB <- cars %>% filter(group == "B")
# summary(carsA)
# summary(carsB)
# Examining the Data, the only difference between the groups we found was that
# Group A has Higher Mean Price than Group B
t.test(price ~ group, data = carsAB, alternative = "less")

# hypothesis seems to be that price will affect our click rate on ads
# lets test this out, group A has significantly less clicks than group B
t.test(total_clicks ~ group, data = carsAB, alternative= "greater")

# Looks like the groups may be split up to see impact of clicks by price
# lets visualize what that looks like
ggplot(na.omit(carsAB), aes(x = scale(total_clicks), y = scale(price), color = group)) +
  geom_point() +
  labs(title = "Clicks by Price in A/B Test (Scaled)") +
  labs(color = "Test Groups") +
  labs(x = "Total Clicks", y = "Price")

#Table of Clicks

table(carsB$clicked)

# Calculating the confidence intervals for each group by total clicks now
# Group A
error_tca <- qt(0.90, df=length(carsA$total_clicks) - 1) * sd(carsA$total_clicks, na.rm = T) / sqrt(length(carsA$total_clicks))
left_tca <- mean(carsA$total_clicks, na.rm = T) - error_tca
right_tca<- mean(carsA$total_clicks, na.rm = T) + error_tca
left_tca; right_tca
# Group B
error_tcb <- qt(0.90, df=length(carsB$total_clicks) - 1) * sd(carsB$total_clicks, na.rm = T) / sqrt(length(carsB$total_clicks))
left_tcb <- mean(carsB$total_clicks, na.rm = T) - error_tcb
right_tcb <- mean(carsB$total_clicks, na.rm = T) + error_tcb
left_tcb; right_tcb
# Calculate Click change based on price in groups
clicksA <- (left_tca+right_tca)/2
clicksB <- (left_tcb+right_tcb)/2
clicksTot <- clicksA + clicksB
conversion <- clicksA/clicksTot - clicksB/clicksTot
rate <- round(conversion * 100, 2)
# Receieved 3.37% less clicks in Group A on Average
rate

# Examine Three Charts Together
# Standard Deviation of Clicks through Days Live
# Median Clicks through Days Live
# Count of Clicks through Days Live
# Again, high amount of observations are on the Hatback and throughout the month decreases
# The abundance of hatchbacks in the early days will skew our A/B Test results for any inference
days_liveGroup <- group_by(days_live, carrosserie, .data = carsAB)
days_clicks <- summarise(days_liveGroup,
                         sd_clicks = sd(total_clicks, na.rm = T),
                         total_clicks=sum(total_clicks),
                         median_clicks = median(total_clicks, na.rm = T),
                         count = n())
p1 <- ggplot(days_clicks) + 
  geom_smooth(aes(x=days_live, y=sd_clicks, color=carrosserie), se = F) + 
  xlim(0,30) +
  labs(color = "Vehicles") +
  labs(x = "Days Live", y = "Deviation of Clicks")
p2 <- ggplot(days_clicks) + 
  geom_smooth(aes(x=days_live, y=median_clicks, color=carrosserie), se = F) + 
  xlim(0,30) +
  labs(color = "Vehicles") +
  labs(x = "Days Live", y = "Median of Clicks")
p3 <- ggplot(days_clicks) + 
  geom_smooth(aes(x=days_live, y=count, color=carrosserie), se = F) + 
  xlim(0,30) +
  labs(color = "Vehicles") +
  labs(x = "Days Live", y = "Count of Clicks")
grid.arrange(p1, p2, p3, ncol = 1)


# Created an interactive graph so we can play with the data
# lets examine the count data for the entire lifecycle
# Hatchbacks highly popular within test groups
cars_count <- ggplot(days_clicks) + 
  geom_smooth(aes(x=days_live, y=count, color=carrosserie), se = F) + 
  xlim(0,150) +
  labs(title = "Clicks per Vehicle by Days Live") +
  labs(color = "Vehicles") +
  labs(x = "Days Live", y = "Count of Clicks") 
ggplotly(cars_count)

# Examine some summary statistics of the Hatcback
# it is below the mean/median price of the group
# it has less power than the average car in the group
# it has less kilometers ran on average even though the age is about the same as group
carsAB %>% filter(carrosserie == "Hatchback (3/5-deurs)") %>% 
  select(photo_cnt, vermogen, price, age, kmstand, group) %>% 
  summary()
carsAB %>% filter(carrosserie != "Hatchback (3/5-deurs)") %>% 
  select(photo_cnt, vermogen, price, age, kmstand, group) %>% 
  summary()

# New table with more evenly distributed car data
carsABRecent <- carsAB %>% filter(carrosserie != "Hatchback (3/5-deurs)")
t.test(total_clicks ~ group, data = carsABRecent)
# Calculate Click change based on price in groups
clicksAB <- 4.872662
clicksBA <- 5.270013
clicksTots <- clicksAB + clicksBA
conversion_smooth <- clicksAB/clicksTots - clicksBA/clicksTots
rate_smooth <- round(conversion_smooth * 100, 2)
# Receieved 3.92% less clicks in Group A on Average
# There is no evidence to Customers in these samples are likely to click on cheap cars
# Even when we remove the cheap Hatchback as a highly popular option
rate_smooth

# Having concluded our A/B Analysis, lets go back to our main data
# After examining the variables, we found that many had a "?" or "None" field as factors
# so clean some of the missing/dirty data from these features
cars$kleur         <- as.factor(ifelse(is.na(cars$kleur ), 
                                       "Other", cars$kleur))
cars$carrosserie   <- as.factor(ifelse(is.na(cars$carrosserie ),
                                       "Other", cars$carrosserie))
cars$aantaldeuren  <- as.factor(ifelse(is.na(cars$kleur ), 
                                       "Other", cars$aantaldeuren))
cars$energielabel  <- as.factor(ifelse(is.na(cars$aantaldeuren ), 
                                       "Other", cars$energielabel))
cars$aantalstoelen <- as.factor(ifelse(is.na(cars$aantalstoelen), 
                                       "Other", cars$aantalstoelen))
cars$photo_cnt     <- as.factor(cars$photo_cnt)
cars$emissie       <- as.numeric(cars$emissie)
# Drop out any price that is unrealistic
# €0 for a car, or 100 million for a volvo, etc.
cars$price <- ifelse(cars$price < quantile(cars$price, 0.05, na.rm = T), NA,
                     ifelse(cars$price > quantile(cars$price, 0.98, na.rm = T), NA, cars$price))

# "Model" alone has no predictive power but combined with the brand it may
# Combine the Brand and Model of Cars
# Now we can drop "Model" as its mostly noise for our algorithm
cars$brand <- str_replace_all(cars$brand, pattern = "[[:punct:]]", "")
cars$brand <- str_replace_all(cars$brand, pattern = "\\s+", " ")
cars$label <- as.factor(paste(cars$brand, cars$model, sep = " "))
cars$label <- str_replace_all(cars$label, pattern = "[[:punct:]]", "")
cars$label <- str_replace_all(cars$label, pattern = "\\s+", " ")
# Let examine our data and see whats popular for our ads
# Format the Cars Labels
cars$label <- as.factor(tolower(cars$label))
all_labels <- str_split(cars$label, " ")
# how many words per label
words_per_label <- sapply(all_labels, length)



# table of frequencies
table(words_per_label)
# to get it as a percent
100 * round(table(words_per_label)/length(words_per_label), 4)


# vector of words in labels
title_words <- unlist(all_labels)
# get unique words
unique_words <- unique(title_words)
num_unique_words <- length(unique(title_words))
# vector to store counts
count_words <- rep(0, num_unique_words)
# count number of occurrences
for (i in 1:num_unique_words) {
  count_words[i] = sum(title_words == unique_words[i])
}
# index values in decreasing order
top_30_order <- order(count_words, decreasing = TRUE)[1:30]
# top 30 frequencies
top_30_freqs <- sort(count_words, decreasing = TRUE)[1:30]
# select top 30 words
top_30_words <- unique_words[top_30_order]



# barplot
# Volkswagen seems to be far ahead of the others
barplot(top_30_freqs, border = NA, names.arg = top_30_words,
        las = 2, ylim = c(0,25000))


# Lets see what vehicle type relates to the highest brand
# similar to our AB test results of Hatchback
# the three most popular cars all have Hatchback types
cars %>% 
  group_by(brand, carrosserie) %>% 
  mutate(count = n()) %>%
  select(brand, carrosserie, count) %>%
  arrange(desc(count)) %>% 
  unique() %>% 
  head()


# Any other features worth creating?
# We can bin certain variables if they are worthwhile
# Vermogen can be split into High,Low Power
# Emissiens can be split into High, Low Emission Cars
cars %>% select(age, price, kmstand, vermogen, days_live, total_clicks, emissie) %>%
  cor(use = "complete.obs") %>% corrplot::corrplot()


# Features to Drop
# model has too many factors with no value, date was used in Age
# l2 is unknown but mostly noise
table(cars$l2)


# Select final features, drop ones we won't use or could cause data leakage
features <- cars %>%
  select(- `group`, - model, - ad_start_dt,
         - src_ad_id, - bouwjaar, - l2, - webclicks, - telclicks, - total_clicks,
         - n_asq, - bids)
str(features)
summary(features)

cars%>%group_by(src_ad_id)%>%summarise(cnt=n())%>%group_by(cnt)%>%summarise(cnt2=n())


#Boxplot 
nums <- unlist(lapply(cars, is.numeric))  
carsnums<- cars[ , nums]

#excluding scr_ad_id
boxplot(carsnums[,-1])

#exluding kmstand, price, annual_kms
boxplot(carsnums[,-c(1,4,7,11,14)])

names(carsnums[,-c(1,4,7,11,14)])

require(ggplot2)
ggplot(data = df.m, aes(x=variable, y=value)) + geom_boxplot(aes(fill=Label))

## Examine the Machine Learning Algorithms we will use
# H2O library was used for performance gains
# Algorithms that can effectively handle NA's were used (RF Imputation was used with no difference)
# Algorithms that can effectively scale were used (YeoJohnson was used with no difference)

cars_h2o <- as.h2o(features)
# Split into Training/Validation/Testing sets
splits <- h2o.splitFrame(data = cars_h2o, ratios = c(0.7, 0.15), seed = 1)
train <- splits[[1]]
validate <- splits[[2]]
test <- splits[[3]]
# Define Label and Predictors
response <- "TenDaysClick"
predictors <- setdiff(names(train), response)




# GBM Algorithm with minor human tuning
# One Hot Encoded Variables as it usually improves RMSE
gbm_fit <- h2o.gbm(x = predictors,
                   y = response,
                   training_frame = train,
                   model_id = "gbm_fit",
                   validation_frame = validate,
                   ntrees = 500,
                   score_tree_interval = 5,
                   stopping_rounds = 3,
                   stopping_metric = "RMSE",
                   stopping_tolerance = 0.0005,
                   categorical_encoding = "OneHotExplicit",
                   seed = 1)
gbm_perf <- h2o.performance(model = gbm_fit,
                            newdata = test)
gbm_perf_train <- h2o.performance(model = gbm_fit,
                            newdata = train)
print(gbm_perf)
print(gbm_perf_train)
# Distributed RandomForest
rf_fit <- h2o.randomForest(x = predictors,
                           y = response,
                           training_frame = train,
                           model_id = "rf_fit",
                           seed = 1,
                           nfolds = 5)
rf_perf <- h2o.performance(model = rf_fit,
                           newdata = test)
rf_perf_train <- h2o.performance(model = rf_fit,
                           newdata = train)
print(rf_perf)
print(rf_perf_train)

# Generalized Linear Model with Gaussian Family
glm_fit <- h2o.glm( x = predictors, 
                    y = response, 
                    training_frame = train,
                    model_id = "glm_fit",
                    validation_frame = validate,
                    family = "gaussian",
                    lambda_search = TRUE)
glm_perf <- h2o.performance(model = glm_fit,
                            newdata = test)
glm_perf_train <- h2o.performance(model = glm_fit,
                                  newdata = train)

print(glm_perf)
print(glm_perf_train)
# Lets examine the results based on RMSE
# RF/GBM performed quite close but GBM was the slowest
# RF had the best RMSE 
# Root Mean Square Error
rfper <-h2o.rmse(rf_perf)  # 7.471413
gbmper <- h2o.rmse(gbm_perf) # 7.70153
glmper <- h2o.rmse(glm_perf) # 10.11961

rfpertr <-h2o.rmse(rf_perf_train)  # 3.467813
gbmpertr <- h2o.rmse(gbm_perf_train) # 7.604088
glmpertr <- h2o.rmse(glm_perf_train) # 10.68962



rf <- cbind(rfper, rfpertr)
gbm <- cbind(gbmper, gbmpertr)
glm <- cbind(glmper, glmpertr)

final <- rbind(rf,gbm,glm)
colnames(final) <- c("Test","Train")
rownames(final) <- c("RF","GBM","GLM")

# Variable Importance
# We see that for GBM the new feature (Age) we created was the most important
# for RF age was the 5th most important

print(gbm_fit@model$variable_importances)
print(rf_fit@model$variable_importances)



# Look at scoring history for GBM model
# How long did it take for us to reach optimal accuracy
plot(gbm_fit, 
     timestep = "number_of_trees", 
     metric = "RMSE")
plot(gbm_fit, 
     timestep = "number_of_trees", 
     metric = "deviance")


# Since GBM was the best performer lets tune it
# Hyper Parameter Tuning 
# First Pass
hyper_params = list( max_depth = seq(1,29,2) ) # Since dataqset is small
grid <- h2o.grid(
  hyper_params = hyper_params,
  ## full Cartesian hyper-parameter search
  search_criteria = list(strategy = "Cartesian"),
  algorithm="gbm",
  grid_id="depth_grid",
  x = predictors, 
  y = response, 
  training_frame = train, 
  validation_frame = validate,
  ## here, use "more than enough" trees - we have early stopping
  ntrees = 10000,                                                            
  ## since we have learning_rate_annealing, we can afford to start with a bigger learning rate
  learn_rate = 0.05,                                                         
  ## learning rate annealing: learning_rate shrinks by 1% after every tree 
  learn_rate_annealing = 0.99,                                               
  sample_rate = 0.8,                                                       
  col_sample_rate = 0.8, 
  seed = 1234,                                                             
  ## early stopping once the validation AUC doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5,
  stopping_tolerance = 1e-4,
  stopping_metric = "RMSE", 
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10                                                
)
## sort the grid models by decreasing AUC
sortedGrid <- h2o.getGrid("depth_grid", sort_by="rmse", decreasing = TRUE) 
## find the range of max_depth for the top 5 models
topDepths = sortedGrid@summary_table$max_depth[1:5]                       
minDepth = min(as.numeric(topDepths))
maxDepth = max(as.numeric(topDepths))



# Now that we know a good range for max_depth, 
# we can tune all other parameters in more detail 
# Since we don’t know what combinations of hyper-parameters will result in the best model, 
# we’ll use random hyper-parameter search 
hyper_params = list( 
  ## restrict the search to the range of max_depth established above
  max_depth = seq(minDepth,maxDepth,1),                                      
  sample_rate = seq(0.2,1,0.01),                                             
  col_sample_rate = seq(0.2,1,0.01),                                         
  col_sample_rate_per_tree = seq(0.2,1,0.01),                                
  col_sample_rate_change_per_level = seq(0.9,1.1,0.01),                      
  min_rows = 2^seq(0,log2(nrow(train))-1,1),                                 
  nbins = 2^seq(4,10,1),                                                     
  nbins_cats = 2^seq(4,12,1),                                                
  min_split_improvement = c(0,1e-8,1e-6,1e-4),                               
  histogram_type = c("UniformAdaptive","QuantilesGlobal","RoundRobin")       
)
search_criteria = list(
  ## Random grid search
  strategy = "RandomDiscrete",      
  ## limit the runtime to 60 minutes
  max_runtime_secs = 3600,         
  ## build no more than 100 models
  max_models = 100,                  
  seed = 1234,                        
  ## early stopping once the leaderboard of the top 5 models is converged to 0.1% relative difference
  stopping_rounds = 5,                
  stopping_metric = "RMSE",
  stopping_tolerance = 1e-3
)
#Continue from here 
grid <- h2o.grid(
  hyper_params = hyper_params,
  search_criteria = search_criteria,
  algorithm = "gbm",
  grid_id = "final_grid", 
  x = predictors, 
  y = response, 
  training_frame = train, 
  validation_frame = validate,
  ntrees = 10000,                                                            
  learn_rate = 0.05,                                                         
  learn_rate_annealing = 0.99,                                               
  max_runtime_secs = 3600,                                                 
  stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "RMSE", 
  score_tree_interval = 10,
  nfolds = 5,
  seed = 1234                                                             
)
## Sort the grid models by RMSE
sortedGrid <- h2o.getGrid("final_grid", sort_by = "RMSE", decreasing = TRUE)    
sortedGrid 


# Choose Best Model
gbm <- h2o.getModel(sortedGrid@model_ids[[1]])
h2o.rmse(h2o.performance(gbm, newdata = test))


# Keeping the same “best” model,
# we can make test set predictions as follows:
preds <- h2o.predict(gbm, test)
head(preds, 10)


# Final GBM R2
gbm@model$validation_metrics@metrics$r2


# Save Model and Predictions
h2o.saveModel(gbm, "/Users/mustafaozturk/Desktop/eBay Case/best_model.csv", force=TRUE)
h2o.exportFile(preds, "/Users/mustafaozturk/Desktop/eBay Case/best_preds.csv", force=TRUE)
