#GT 4801 Project Code
# Sara Stefan and Samuel Hodges



#load packages

install.packages("tidyverse") # a set of data science tools including dplyr, tidyr and stringr
install.packages("skimr") # a package to facilitate data summaries
install.packages("Hmisc") # a package for data analysis
install.packages("plm")

library(tidyverse)
library(skimr)
library(Hmisc)
library(plm)
library(ggplot2)

#load data
death_data <- read.csv(file.choose(), header = TRUE)

#view data
View(death_data)
summary(death_data)


# Data cleaning / transformations
cleanDeaths <- death_data[complete.cases(death_data),]  # only select complete data rows for the cleandata frame
cleanDeaths <- unique(cleanDeaths) # eliminate duplicate values (none in our example file)


View(cleanDeaths)

# covert characters to factors

cleanDeaths$Leading.Cause <- as.factor(cleanDeaths$Leading.Cause)
cleanDeaths$Sex <- as.factor(cleanDeaths$Sex)
cleanDeaths$Race.Ethnicity <- as.factor(cleanDeaths$Race.Ethnicity)


#remove the rows containing "." in the Deaths, Death.Rate, and Age.Adjusted.Death.Rate columns from dataset
cleanDeaths<-cleanDeaths[!(cleanDeaths$Deaths=="." | cleanDeaths$Death.Rate=="." | cleanDeaths$Age.Adjusted.Death.Rate=="."),]
View(cleanDeaths)


#confirm the "."s were removed
summary(cleanDeaths)


#remove NAs
complete_cleanDeaths <- na.omit(cleanDeaths)

# convert to numeric now that NAs have been removed
complete_cleanDeaths$Deaths <- as.numeric(complete_cleanDeaths$Deaths)
complete_cleanDeaths$Death.Rate <- as.numeric(complete_cleanDeaths$Death.Rate)
complete_cleanDeaths$Age.Adjusted.Death.Rate <- as.numeric(complete_cleanDeaths$Age.Adjusted.Death.Rate)

complete_cleanDeaths <- unique(complete_cleanDeaths) # eliminate duplicate values

#view cleaned data
summary(complete_cleanDeaths)
View(complete_cleanDeaths)

skim(complete_cleanDeaths)

# Histogram
ggplot(data=complete_cleanDeaths, aes(complete_cleanDeaths$Death.Rate)) + geom_histogram()


#boxplot with leading cause of death and number of deaths
complete_cleanDeaths %>% 
  ggplot(aes(Leading.Cause, y=Deaths)) + geom_boxplot()

#boxplot with race/ethnicity and number of deaths
complete_cleanDeaths %>% 
  ggplot(aes(Race.Ethnicity, y=Deaths)) + geom_boxplot()

# Scatterplot comparing two variables
# Base R

#explortatory analysis - a couple graphs to try
plot(complete_cleanDeaths$Leading.Cause,complete_cleanDeaths$Sex)

ggplot(complete_cleanDeaths, aes(x=Race.Ethnicity, y=Leading.Cause)) + geom_point()

# trying a linear regression
myreg1 <- lm(formula = Leading.Cause ~ Race.Ethnicity + Sex, data = complete_cleanDeaths)
summary(myreg1)

# because Leading.Cause is not numeric, it cannot be used as the dependent variable in our linear regression,
# which is what we wanted to do. So we will do a few different models instead

#try out some histograms
hist(complete_cleanDeaths$Death.Rate) 
hist(complete_cleanDeaths$Deaths)
hist(complete_cleanDeaths$Leading.Cause)
hist(complete_cleanDeaths$Age.Adjusted.Death.Rate)

# trying out some different regression models

model1 <- lm(formula = Death.Rate ~ Sex + Race.Ethnicity , data = complete_cleanDeaths)
summary(model1)

model2 <- lm(formula = Deaths ~ Race.Ethnicity, data = complete_cleanDeaths)
summary(model2)



