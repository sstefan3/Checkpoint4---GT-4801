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

complete_cleanDeaths$Leading.Cause <- as.factor(cleanDeaths$Leading.Cause)
complete_cleanDeaths$Sex <- as.factor(cleanDeaths$Sex)
complete_cleanDeaths$Race.Ethnicity <- as.factor(cleanDeaths$Race.Ethnicity)


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
summary(complete_cleanDeaths)
View(complete_cleanDeaths)

## shorten cause of Death names
complete_cleanDeaths <- complete_cleanDeaths %>%
  mutate(Leading.Cause = str_replace(Leading.Cause, "Malignant Neoplasms \\(Cancer: C00-C97\\)", "Cancer")) %>%
  mutate(Leading.Cause = str_replace(Leading.Cause, "Accidents Except Drug Poisoning \\(V01-X39, X43, X45-X59, Y85-Y86\\)", "Accidents")) %>%
  mutate(Leading.Cause = str_replace(Leading.Cause, "Alzheimer's Disease \\(G30\\)", "Alzheimer's")) %>%
  mutate(Leading.Cause = str_replace(Leading.Cause, "Anemias \\(D50-D64\\)", "Anemias")) %>%
  mutate(Leading.Cause = str_replace(Leading.Cause, "Aortic Aneurysm and Dissection \\(I71\\)", "Aneurysm")) %>%
  mutate(Leading.Cause = str_replace(Leading.Cause, "Assault \\(Homicide: U01-U02, Y87.1, X85-Y09\\)", "Homicide")) %>%
  mutate(Leading.Cause = str_replace(Leading.Cause, "Assault \\(Homicide: Y87.1, X85-Y09\\)","Homicide")) %>%
  mutate(Leading.Cause = str_replace(Leading.Cause, "Atherosclerosis \\(I70\\)", "Atherosclerosis")) %>%
  mutate(Leading.Cause = str_replace(Leading.Cause, "Cerebrovascular Disease \\(Stroke: I60-I69\\)", "Stroke")) %>%
  mutate(Leading.Cause = str_replace(Leading.Cause, "Certain Conditions originating in the Perinatal Period \\(P00-P96\\)", "Perinatal Period")) %>%
  mutate(Leading.Cause = str_replace(Leading.Cause, "Chronic Liver Disease and Cirrhosis \\(K70, K73-K74\\)", "Liver Disease")) %>%
  mutate(Leading.Cause = str_replace(Leading.Cause, "Chronic Liver Disease and Cirrhosis \\(K70, K73\\)", "Liver Disease")) %>%
  mutate(Leading.Cause = str_replace(Leading.Cause, "Chronic Lower Respiratory Diseases \\(J40-J47\\)", "Respiratory Disease")) %>%
  mutate(Leading.Cause = str_replace(Leading.Cause, "Congenital Malformations, Deformations, and Chromosomal Abnormalities \\(Q00-Q99\\)", "Chromosomal Abnormalities")) %>%
  mutate(Leading.Cause = str_replace(Leading.Cause, "Diabetes Mellitus \\(E10-E14\\)", "Diabetes")) %>%
  mutate(Leading.Cause = str_replace(Leading.Cause, "Diseases of Heart \\(I00-I09, I11, I13, I20-I51\\)", "Heart Disease")) %>%
  mutate(Leading.Cause = str_replace(Leading.Cause, "Essential Hypertension and Renal Diseases \\(I10, I12\\)", "Renal Disease")) %>%
  mutate(Leading.Cause = str_replace(Leading.Cause, "Human Immunodeficiency Virus Disease \\(HIV: B20-B24\\)", "HIV")) %>%
  mutate(Leading.Cause = str_replace(Leading.Cause, "Influenza \\(Flu\\) and Pneumonia \\(J09-J18\\)", "Flu/Pneumonia")) %>%
  mutate(Leading.Cause = str_replace(Leading.Cause, "Insitu or Benign / Uncertain Neoplasms \\(D00-D48\\)", "Neoplasms")) %>%
  mutate(Leading.Cause = str_replace(Leading.Cause, "Intentional Self-Harm \\(Suicide: U03, X60-X84, Y87.0\\)", "Suicide")) %>%
  mutate(Leading.Cause = str_replace(Leading.Cause, "Intentional Self-Harm \\(Suicide: X60-X84, Y87.0\\)", "Suicide")) %>%
  mutate(Leading.Cause = str_replace(Leading.Cause, "Meningitis \\(G00, G03\\)", "Meningitis")) %>%
  mutate(Leading.Cause = str_replace(Leading.Cause, "Mental and Behavioral Disorders due to Accidental Poisoning and Other Psychoactive Substance Use \\(F11-F16, F18-F19, X40-X42, X44\\)", "Drug Use Disorder")) %>%
  mutate(Leading.Cause = str_replace(Leading.Cause, "Mental and Behavioral Disorders due to Use of Alcohol \\(F10\\)", "Alcohol Use Disorder")) %>%
  mutate(Leading.Cause = str_replace(Leading.Cause, "Nephritis, Nephrotic Syndrome and Nephrisis \\(N00-N07, N17-N19, N25-N27\\)", "Nephritis")) %>%
  mutate(Leading.Cause = str_replace(Leading.Cause, "Parkinson's Disease \\(G20\\)", "Parkinson's")) %>%
  mutate(Leading.Cause = str_replace(Leading.Cause, "Peptic Ulcer \\(K25-K28\\)", "Ulcer")) %>%
  mutate(Leading.Cause = str_replace(Leading.Cause, "Pregnancy, Childbirth and the Puerperium \\(O00-O09\\)", "Childbrith Complications")) %>%
  mutate(Leading.Cause = str_replace(Leading.Cause, "Septicemia \\(A40-A41\\)", "Septicemia")) %>%
  mutate(Leading.Cause = str_replace(Leading.Cause, "Tuberculosis \\(A16-A19\\)", "Tuberculosis")) %>%
  mutate(Leading.Cause = str_replace(Leading.Cause, "Viral Hepatitis \\(B15-B19\\)", "Hepatitis")) %>%
  mutate(Race.Ethnicity = str_replace(Race.Ethnicity, "Non-Hispanic White", "White Non-Hispanic")) %>%
  mutate(Race.Ethnicity = str_replace(Race.Ethnicity, "Non-Hispanic Black", "Black Non-Hispanic"))
  

#view cleaned data
summary(complete_cleanDeaths)
View(complete_cleanDeaths)
#convert numbers from characters to numeric
complete_cleanDeaths$Deaths <- as.numeric(complete_cleanDeaths$Deaths)
complete_cleanDeaths$Death.Rate <- as.numeric(complete_cleanDeaths$Death.Rate)
complete_cleanDeaths$Age.Adjusted.Death.Rate <- as.numeric(complete_cleanDeaths$Age.Adjusted.Death.Rate)
#conver characters to factors
complete_cleanDeaths$Leading.Cause <- as.factor(complete_cleanDeaths$Leading.Cause)
complete_cleanDeaths$Sex <- as.factor(complete_cleanDeaths$Sex)
complete_cleanDeaths$Race.Ethnicity <- as.factor(complete_cleanDeaths$Race.Ethnicity)
#remove NAs
complete_cleanDeaths <- na.omit(complete_cleanDeaths)
#view cleaned data
summary(complete_cleanDeaths)
View(complete_cleanDeaths)
skim(complete_cleanDeaths)

# sorry, a lot of the stuff above was repeated because I was trying to make sure everything was working right

# Histogram
ggplot(data=complete_cleanDeaths, aes(complete_cleanDeaths$Death.Rate)) + geom_histogram()


#boxplot with leading cause of death and number of deaths
complete_cleanDeaths %>% 
  ggplot(aes(Leading.Cause, y=Deaths)) + geom_boxplot()
#boxplot with race/ethnicity and number of deaths
complete_cleanDeaths %>% 
  ggplot(aes(Race.Ethnicity, y=Deaths)) + geom_boxplot()

#boxplot with race/ethnicity and age adjusted death rate
complete_cleanDeaths %>% 
  ggplot(aes(Race.Ethnicity, y=Age.Adjusted.Death.Rate)) + geom_boxplot()

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

model3 <- lm(formula = Deaths ~ Leading.Cause, data = complete_cleanDeaths)
summary(model3)

model4 <- lm(formula = Death.Rate ~ Leading.Cause, data = complete_cleanDeaths)
summary(model4)

##scatter plot with regression lines for model 2 
plot(complete_cleanDeaths$Race.Ethnicity, complete_cleanDeaths$Deaths, main = "Race and Ethnicity and Deaths", xlim = c(0, 5),  
     ylim = c(0, 4000), xlab = "Race and Ethnicity",  
     ylab = "Deaths")  
#plot for model 3
plot(complete_cleanDeaths$Leading.Cause, complete_cleanDeaths$Deaths, main = "Leading Cause of Death and Deaths", xlim = c(0, 21),  
     ylim = c(0, 4500), xlab = "Leading Cause",  
     ylab = "Deaths")  
# plot for model 4
plot(complete_cleanDeaths$Leading.Cause, complete_cleanDeaths$Death.Rate, main = "Leading Cause of Death and Death Rates", xlim = c(0, 21),  
     ylim = c(0, 450), xlab = "Leading Cause",  
     ylab = "Death Rate")  




