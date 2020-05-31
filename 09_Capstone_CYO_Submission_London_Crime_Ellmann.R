#################################################################################################
# HarvardX Data Science Professional Certificate
# PH125.9x: Choose Your Own Project Submission
# May 2020
# Author: Erdmuthe Ellmann
#################################################################################################


# install packages if needed
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org") 
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
if(!require(forecast)) install.packages("forecast", repos = "http://cran.us.r-project.org")
if(!require(broom)) install.packages("broom", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")

# load libraries
library(tidyverse)
library(caret)
library(data.table)
library(ggplot2)
library(lubridate)
library(ggrepel)
library(forecast)
library(broom)
library(rpart)
library(kableExtra)

# disable scientific notation
options(scipen = 999)      


#################################################################################################
# Get the Dataset
#################################################################################################

# Both the London Crime dataset and the London Boroughs dataset were originally downloaded
# from Kaggle:
# https://www.kaggle.com/jboysen/london-crime
# https://www.kaggle.com/marshald/london-boroughs


# For this project both datasets can be downloaded from my github repository. I've also tried to
# figure out how to code a download from Kaggle, but couldn't make the code work.

################################################
# A) code to download from Github
################################################

# The two files are in a subfolder "/data" and have to be downloaded and unzipped

# download each file from github
dl <- tempfile()
download.file("https://github.com/ellmanne/LondonCrime/raw/master/data/londoncrime2016.zip", dl)
londoncrime2016_csv <- unzip(dl, "londoncrime2016.csv")
londoncrime_temp <- read.csv(londoncrime2016_csv, stringsAsFactors = FALSE)

dl <- tempfile()
download.file("https://github.com/ellmanne/LondonCrime/raw/master/data/london-boroughs.zip", dl)
londonboroughs_csv <- unzip(dl, "london-borough-profiles-2016 Data set.csv")
londonboroughs_temp <- read.csv(londonboroughs_csv, stringsAsFactors = FALSE)

# remove the files that are not needed anymore
rm(londoncrime2016_csv, londonboroughs_csv)


################################################
# B) code to download from Kaggle (doesn't work)
################################################
#
# I've also tried to provide code to directly download from kaggle, but although I've tried for 
# several hours I couldn't make the code work. It's from this stackoverflow question:
# https://stackoverflow.com/questions/35303779/downloading-kaggle-zip-files-in-r
# Would have been really cool if it would have worked, because then anyone with a kaggle account
# could have downloaded the original dataset directly from kaggle in one go.
# 
# if(!require(RCurl)) install.packages("RCurl", repos = "http://cran.us.r-project.org")
# library(RCurl)
# 
# # Set your browsing links
# loginurl <- "https://www.kaggle.com/account/login?phase=emailSignIn&returnUrl=https%3A%2F%2Fwww.kaggle.com%2F"
# dataurl <- "https://www.kaggle.com/jboysen/london-crime/data#london_crime_by_lsoa.csv"
# 
# # Set user account data and agent
# pars <- list(
#   UserName <- "---write your user name here---",
#   Password <- "---write your pw here---"
# )
# 
# agent <- "Chrome" # or whatever
# 
# # Set RCurl pars
# curl <-  getCurlHandle()
# # curlSetOpt(cookiejar = "cookies.txt",  useragent = agent, followlocation = TRUE, curl = curl)
# # Also if you do not need to read the cookies.
# curlSetOpt(cookiejar = "", useragent = agent, followlocation = TRUE, curl = curl)
# 
# ?postForm
# 
# # Post login form
# welcome <- postForm(loginurl, .params = pars, curl = curl)
# 
# bdown <- function(url, file, curl){
#   f = CFILE(file, mode = "wb")
#   curlPerform(url = url, writedata = f@ref, noprogress = FALSE, curl = curl)
#   close(f)
# }
# 
# ret = bdown(dataurl, "c:\\test.csv", curl)
# 
# rm(curl)
# gc()
# ##############################


#################################################################################################
# Data Preparation
#################################################################################################

################################################
# Create Combined Dataset
################################################

# add a new column to londoncrime which combines year and month
londoncrime_temp$date <- with(londoncrime_temp, sprintf("%d-%02d", year, month))

# aggregate the dataset without the first two columns 
# (one is just an index and lsoa_code is not needed and just makes the dataset much bigger)
londoncrime_temp <- londoncrime_temp %>%
  group_by(borough, major_category, minor_category, year, month, date) %>%
  summarize(value = sum(value)) %>%
  select(borough, major_category, minor_category, value, year, month, date)

# change class into tibble
londoncrime_temp <- as_tibble(londoncrime_temp)

# check the resulting dataset
head(londoncrime_temp)

# Londoncrime dataset only contains 7 variables, one of them is about the area the crime happened
# (borough), two describing the crime (major and minor category), two about the month and year
# the crime happened and lastly the number of crimes (value).

# check dimensions of london borouhgs dataset
dim(londonboroughs_temp)

# check london boroughs dataset
head(londonboroughs_temp)
unique(londonboroughs_temp$Area.name)
n_distinct(londonboroughs_temp$Area.name)
n_distinct(londoncrime_temp$borough)

# extract the relevant columns from the dataset and rename them
londonboroughs <- londonboroughs_temp %>%
  select(Area.name, GLA.Population.Estimate.2016, 
         GLA.Household.Estimate.2016, Inland.Area..Hectares., 
         Population.density..per.hectare..2016, Average.Age..2016,
         Proportion.of.population.aged.0.15..2016, 
         Proportion.of.population.of.working.age..2016,
         Proportion.of.population.aged.65.and.over..2016, 
         Employment.rate......2015., Male.employment.rate..2015., 
         Female.employment.rate..2015., 
         Unemployment.rate..2015.) %>%
  setnames(c("Area.name", "GLA.Population.Estimate.2016", 
              "GLA.Household.Estimate.2016", "Inland.Area..Hectares.", 
              "Population.density..per.hectare..2016", "Average.Age..2016",
              "Proportion.of.population.aged.0.15..2016", 
              "Proportion.of.population.of.working.age..2016",
              "Proportion.of.population.aged.65.and.over..2016", 
              "Employment.rate......2015.", "Male.employment.rate..2015.", 
              "Female.employment.rate..2015.", 
              "Unemployment.rate..2015."),
            c("borough", "population", "household", "hectares", "population_density", 
              "avg_age", "population_under_16", "population_working_age", "population_over_64", 
              "employment_rate_2015", "employment_rate_male_2015", "employment_rate_female_2015", 
              "unemployment_rate_2015"))

# remove the first row (this row is blank)
londonboroughs <- londonboroughs[-1,]

# remove the last 6 rows (these rows contain additional data for total UK, England etc., which we
# don't need)
londonboroughs <- londonboroughs[-c(34:39),]

# replace the "." with "NA"
londonboroughs[londonboroughs == "."] <- NA

# remove whitespace infront of the numbers in the columns population and household
londonboroughs <- londonboroughs %>% 
  mutate_at(2:3, str_trim)

# set format to numeric for the columns which are saved as characters at the moment (parse_number
# removes the commas and converts to numeric in one function)
londonboroughs <- londonboroughs %>% 
  mutate_at(2:5, parse_number)

londonboroughs <- londonboroughs %>% 
  mutate_at(11:13, parse_number)

# check the resulting dataset
str(londonboroughs)
head(londonboroughs)

# add the information of the londonboroughs dataset to the londoncrime dataset
londoncrime <- left_join(londoncrime_temp, londonboroughs, by = "borough")

# check the resulting dataset
head(londoncrime)

# remove the temporary datasets
rm(londonboroughs_temp, londoncrime_temp)


################################################
# Missing value: employment rate male for borough "City of London"
################################################

# show employment rate and employment rate 2015 for all boroughs
londoncrime %>%
  group_by(borough) %>%
  summarize(employment_rate_2015 = unique(employment_rate_2015), 
            employment_rate_male_2015 = unique(employment_rate_male_2015), 
            delta = employment_rate_male_2015 - employment_rate_2015,
            delta_prcnt = (delta / employment_rate_2015 * 100)) 

# how to deal with the missing value?
# as we have only such a small number of boroughs, we don't want to just remove City of London
# from the dataset, so we have to somehow predict the employment rate male for this borough to 
# at least get an estimate 
# all male employment rates are higher than the overall employment rate, so we are going to use
# this insight to estimate the male employment rate
# prediction: employment_rate_2015 for City of London + "adder"
# adder: average of the delta percent, that the other boroughs show (difference between male and 
# overall employment rate)


# compute difference between overall employment rate and employment rate male for all boroughs
tab <- londoncrime %>%
  group_by(borough) %>%
  summarize(employment_rate_2015 = unique(employment_rate_2015), 
            employment_rate_male_2015 = unique(employment_rate_male_2015)) %>%
  mutate(delta = employment_rate_male_2015 - employment_rate_2015,
            delta_prop = (delta / employment_rate_2015))

tab

# compute estimate for City of London
estimate_CoL <- tab %>%
  filter(borough != "City of London") %>%
  summarize(estimate = 1 + mean(delta_prop)) %>%
  .$estimate

estimate_CoL

# add the estimated employment rate male to the dataset
londoncrime <- londoncrime %>%
  mutate(employment_rate_male_2015 = case_when(
    borough == "City of London" ~ employment_rate_2015 * estimate_CoL,
    TRUE ~ employment_rate_male_2015))

# check the resulting dataset
londoncrime %>%
  group_by(borough) %>%
  summarize(employment_rate_2015 = unique(employment_rate_2015), 
            employment_rate_male_2015 = unique(employment_rate_male_2015))

# remove the temporary datasets and variables
rm(tab, estimate_CoL)


################################################
# Split the dataset into training, test and validation sets
################################################

# similar to the MovieLens validation, we will first separate a validation dataset to evaluate our
# final algorithm against

# create auxiliary table for boroughs
boroughs <- londoncrime %>%
  group_by(borough) %>%
  summarize(crimes = sum(value))

# randomly select 6 of the 33 boroughs
set.seed(9)
index_validation <- boroughs[sample(nrow(boroughs), 6), ] %>%
  .$borough

# seperate the data of the randonly selected boroughs to get the validation dataset
validation <- londoncrime %>%
  filter(borough %in% index_validation)

# remove the boroughs in the validation set from the list of boroughs
boroughs <- boroughs %>%
  filter(!borough %in% index_validation)

# then do the same to split the remaining dataset into a train and test set we can use to build
# our model
index_test <- boroughs[sample(nrow(boroughs), 6), ] %>%
  .$borough

test <- londoncrime %>%
  filter(borough %in% index_test)

# create the train set with the rest of the data
train <- londoncrime %>%
  filter(!borough %in% c(index_validation, index_test))

# check that separation went fine
dim(train)
dim(test)
dim(validation)

nrow(test) + nrow(train) + nrow(validation) == nrow(londoncrime)


# double check that split into validation, train and test set is ok as far as % of the original
# dataset is concerned -> both make up about 20% of the original london crime dataset
nrow(validation) / (nrow(londoncrime)) * 100
nrow(test) / (nrow(londoncrime)) * 100
nrow(test) / (nrow(train) + nrow(test)) * 100




#################################################################################################
# Exploratory Data Analysis
#################################################################################################

# Project goal: predict number of crimes per month and borough
# EDA: find out more about the dataset and especially find things that could help us with 
# predictions (e.g. suitable prediction method, important predictors)


################################################
# Get a First Impression of the Dataset
################################################

dim(train)
head(train)
str(train)

# Unique major and minor categories and unique boroughs
train %>% 
  summarize(n_major = n_distinct(major_category),
            n_minor = n_distinct(minor_category),
            n_borough = n_distinct(borough))

unique(train$major_category)
unique(train$minor_category)
unique(train$borough)

# average crimes per month
avg_crimes <- train %>% 
  summarize(avg_crimes = sum(value) / n_distinct(date)) %>% 
  .$avg_crimes

avg_crimes


# number of crimes per month
train %>% 
  group_by(month) %>%
  summarize(crimes = sum(value)) %>%
  ggplot(aes(month, crimes)) +
  geom_line(color = "steelblue4", size = 1) +
  geom_abline(intercept = avg_crimes, color = "grey", lty = 2) + 
  ylim(0, 50000) +
  scale_x_continuous(breaks = c(1:12)) +
  theme_classic() +
  labs(title = "Number of Crimes per Month and Period Average",
       x = "Month", y = "Number of Crimes") 

# number of crimes per month vary a bit with 
# ...higher Jan, Mar, May, Jul, Oct, Dec 
# ...and lower Feb, Apr, Jun, Aug, Sep, Nov
# however, overall there seems to be an increasing trend as well (Jan-Apr below avg, as of May
# above avg)

# could the monthly up and down pattern have to do with different number of days per month?

# calculate average crimes per day for each month
crimes_per_day <- train %>%
  group_by(month) %>%
  summarize(crimes = sum(value)) %>%
  select(month, crimes) %>%
  mutate(days = c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31), avg_crimes_day = crimes / days)

# plot average crimes per day for each month
crimes_per_day %>%
  ggplot(aes(month, avg_crimes_day)) +
  geom_line(color = "steelblue4", size = 1) +
  ylim(0, 2000) +
  scale_x_continuous(breaks = c(1:12)) +
  theme_classic() +
  labs(title = "Average Crimes per Day for each Month",
       x = "Month", y = "Number of Crimes") 

# same chart but with smaller scale to see more detail
crimes_per_day %>%
  ggplot(aes(month, avg_crimes_day)) +
  geom_line(color = "steelblue4", size = 1) +
  ylim(1000, 1500) +
  scale_x_continuous(breaks = c(1:12)) +
  theme_classic() +
  labs(title = "Average Crimes per Day for each Month",
       x = "Month", y = "Number of Crimes") 

# some of the variation seems to have levelled out if one considers the different number of days
# now the pattern more looks like it could be driven by school holidays (e.g. Easter in Mar/Apr,
# summer holidays in Aug) and seasonality, maybe due to weather conditions
# however, this is not the focus of the project and we don't have the necessary data to further
# investigate this, so we'll leave it at that

# number of crimes per borough 
train %>% 
  group_by(borough) %>%
  summarize(crimes = sum(value)) %>%
  ggplot(aes(reorder(borough, -crimes), crimes)) +
  geom_bar(stat = "identity", fill = "steelblue4") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Number of Crimes per Borough",
       x = "", y = "Number of Crimes")

# Westminster is the borough with the highest number of crimes, followed by Southwark and Newham
# Kingston upon Thames has the lowest number of crimes

# number of crimes per month and borough (faceted line chart, descending order)
train %>% 
  group_by(month, borough) %>%
  summarize(crimes = sum(value)) %>%
  mutate(borough = reorder(borough, -crimes)) %>%
  ggplot(aes(month, crimes, color = borough)) +
  geom_line(show.legend = FALSE, size = 1) +
  ylim(c(0, 5000)) +
  scale_x_continuous(breaks = c(1:12)) +
  facet_wrap(~borough) +
  theme_minimal() +
  labs(title = "Number of Crimes per Month and Borough",
       x = "Month", y = "Number of Crimes")

# overall, the number of crimes in each borough seems to be relatively stable over those 12 months
# only single spikes, like in Westminster in Jul, Aug and Dec are striking
# however, boroughs seem to be quite different, some range around 1.000 crimes per month, some
# show much higher numbers, e.g. Westminster which ranges around 4.000 crimes per month



################################################
# Explore the Dataset in Detail
################################################

# analyze groups of variables:
#  - major and minor category
#  - population, household, hectares and population density
#  - age, age under 16, working age, age over 64
#  - employment/unemployment rates


# number of crimes by major category
train %>% 
  group_by(major_category) %>%
  summarize(crimes = sum(value)) %>%
  arrange(desc(crimes))

train %>% 
  group_by(major_category) %>%
  summarize(crimes = sum(value)) %>%
  ggplot(aes(reorder(major_category, -crimes), crimes)) +
  geom_bar(stat = "identity", fill = "steelblue4") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Number of Crimes by Major Category",
       x = "", y = "Number of Crimes")

# by far the highest number of crimes are Theft and Handling as well as Violence Against the Person
# there are no crimes documented with major category Fraud or Forgery and Sexual Offences


# number of crimes by minor category
train %>% 
  group_by(minor_category) %>%
  summarize(crimes = sum(value)) %>%
  arrange(desc(crimes))

train %>% 
  group_by(minor_category) %>%
  summarize(crimes = sum(value)) %>%
  ggplot(aes(reorder(minor_category, -crimes), crimes)) +
  geom_bar(stat = "identity", fill = "steelblue4") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Number of Crimes by Minor Category",
       x = "", y = "Number of Crimes")

# Other Theft is the minor category with the highest number of crimes, followed by Harassment
# and Common Assault, which belong to the major category Violence Against the Person


# number of crimes by major and minor category
train %>% 
  group_by(major_category, minor_category) %>%
  summarize(crimes = sum(value))

train %>% 
  group_by(major_category, minor_category) %>%
  filter(!major_category %in% c("Fraud or Forgery", "Sexual Offences")) %>%
  summarize(crimes = sum(value)) %>%
  ggplot(aes(reorder(minor_category, -crimes), crimes, color = major_category, 
             fill = major_category)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Number of Crimes by Major and Minor Category",
       x = "", y = "Number of Crimes")


# number of crimes per major category and month (without Fraud or Forgery and Sexual Offences as 
# these are zero and would just unnecessarily clutter the chart)
train %>% 
  group_by(month, major_category) %>%
  filter(!major_category %in% c("Fraud or Forgery", "Sexual Offences")) %>%
  summarize(crimes = sum(value)) %>%
  ggplot(aes(month, crimes, color = major_category)) +
  geom_line(size = 1) +
  scale_color_discrete(name = "Major Category") +
  scale_x_continuous(breaks = c(1:12)) +
  theme_classic() +
  labs(title = "Number of Crimes per Month",
       x = "Month", y = "Number of Crimes") 

# FYI - tried for two hours to replace the legend with data labels to the right of the lines in 
# the same color as the lines, but couldn't make anything work properly (all solutions repeated 
# the labels for each month, so you could only see labels all over the place)

# especially Theft and Handling seems to be driving the overall trend of a rising number of crimes
# Violence Against the Person also contributes a lot to the rising number of crimes during the 
# middle of the year
# the other five categories are relatively stable over time, only Burglary and Criminal Damage
# have switched places over time



### population size and crimes

# subset with population per 100.000 residents
crime_and_population <- train %>% 
  group_by(borough) %>%
  summarize(crimes = sum(value), population = unique(population)) %>%
  mutate(crimes_per_100.000 = (crimes / population) * 100000) %>%
  arrange(desc(crimes_per_100.000))

crime_and_population

crime_and_population %>% 
  ggplot(aes(reorder(borough, -crimes_per_100.000), crimes_per_100.000)) +
  geom_bar(stat = "identity", fill = "steelblue4") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Number of Crimes per 100.000 Residents",
       x = "", y = "Number of Crimes") 

# distribution is only a little bit different compared to absolute number of crimes per borough



# number of crimes and population size for each borough
train %>% 
  group_by(borough) %>%
  summarize(crimes = sum(value), population = unique(population)) %>%
  ggplot(aes(population, crimes, color = borough)) +
  geom_point(size = 4) +
  theme_classic() +
  xlim(c(0, 400000)) +
  ylim(c(0, 50000)) +
  geom_text_repel(aes(label = borough), size = 3.5, hjust = -0.2) +
  theme(legend.position = "none") + 
  labs(title = "Number of Crimes and Population Size",
       x = "Population", y = "Number of Crimes") 

# highly populated boroughs have a higher number of documented crimes


# number of crimes and households for each borough
train %>% 
  group_by(borough) %>%
  summarize(crimes = sum(value), household = unique(household)) %>%
  ggplot(aes(household, crimes, color = borough)) +
  geom_point(size = 4) +
  theme_classic() +
  xlim(c(0, 150000)) +
  ylim(c(0, 50000)) +
  geom_text_repel(aes(label = borough), size = 3.5, hjust = -0.2) +
  theme(legend.position = "none") + 
  labs(title = "Number of Crimes and Households",
       x = "Households", y = "Number of Crimes") 

# same applied to households -> there seems to be a correlation between high number of households
# and high number of crimes


# number of crimes and population density for each borough
train %>% 
  group_by(borough) %>%
  summarize(crimes = sum(value), population_density = unique(population_density)) %>%
  ggplot(aes(population_density, crimes, color = borough)) +
  geom_point(size = 4) +
  xlim(c(0, 180)) +
  ylim(c(0, 50000)) +
  theme_classic() +
  geom_text_repel(aes(label = borough), size = 3.5, hjust = -0.2) +
  theme(legend.position = "none") + 
  labs(title = "Number of Crimes and Population Density",
       x = "Population Density", y = "Number of Crimes") 

# the correlation seems to be even more clear for population density


# number of crimes and hectares for each borough
train %>% 
  group_by(borough) %>%
  summarize(crimes = sum(value), hectares = unique(hectares)) %>%
  ggplot(aes(hectares, crimes, color = borough)) +
  geom_point(size = 4) +
  xlim(c(0, 20000)) +
  ylim(c(0, 50000)) +
  theme_classic() +
  geom_text_repel(aes(label = borough), size = 3.5, hjust = -0.2) +
  theme(legend.position = "none") + 
  labs(title = "Number of Crimes and Hectares",
       x = "Hectares", y = "Number of Crimes") 

# hectares seems like a less promising indicator for high number of crimes compared to population,
# households or population density 
# e.g. Hillingdon has about the same number of crimes as Islington but they have completely 
# different hectares


# correlation coefficient

# In statistics, the correlation coefficient r measures the strength and direction of a linear 
# relationship between two variables on a scatterplot. The value of r is always between +1 and –1. 
# 0 = no relationship

# calculate correlation coefficient for population variables
train %>% 
  group_by(borough) %>%
  summarize(crimes = sum(value), population = unique(population), 
            household = unique(household), population_density = unique(population_density), 
            hectares = unique(hectares)) %>%
  summarize(cor(population, household), cor(population, population_density),
            cor(population, hectares), cor(household, population_density), 
            cor(household, hectares), cor(population_density, hectares))

# high positive correlation between population and households
# very weak negative correlation between population and population density
# medium positive correlation between population and hectares
# weak positive correlation between households and population density
# medium positive correlation between households and hectares 
# strong negative correlation between population density and hectares

# calculate correlation coefficient for crimes vs. population variables
train %>% 
  group_by(borough) %>%
  summarize(crimes = sum(value), population = unique(population), 
            household = unique(household), population_density = unique(population_density), 
            hectares = unique(hectares)) %>%
  summarize(cor(crimes, population), cor(crimes, household), cor(crimes, population_density),
            cor(crimes, hectares))

# the strongest correlation seems to be between crimes and households, followed by crimes and 
# population density
# crimes and hectares seem to have only a weak negative correlation
# population and households are very strongly correlated with each other, so they seem to be two 
# slightly different measures for the same issue, thus we're just going to include one of them 
# for the prediction model (which is households, as the correlation to crimes is the strongest)




### population age and crimes

# number of crimes and average age of the population
train %>% 
  group_by(borough) %>%
  summarize(crimes = sum(value), avg_age = unique(avg_age)) %>%
  ggplot(aes(avg_age, crimes, color = borough)) +
  geom_point(size = 4) +
  theme_classic() +
  geom_text_repel(aes(label = borough), size = 3.5, hjust = -0.2) +
  xlim(c(0, 100)) +
  ylim(c(0, 50000)) +
  theme(legend.position = "none") + 
  labs(title = "Number of Crimes and Average Age of the Population",
       x = "Average Age", y = "Number of Crimes") 

# there could be a weak negative correlation between average age and crimes
# Westminster seems to be an outlier


# number of crimes and population proportion under 16
train %>% 
  group_by(borough) %>%
  summarize(crimes = sum(value), population_under_16 = unique(population_under_16)) %>%
  ggplot(aes(population_under_16, crimes, color = borough)) +
  geom_point(size = 4) +
  theme_classic() +
  geom_text_repel(aes(label = borough), size = 3.5, hjust = -0.2) +
  theme(legend.position = "none") + 
  xlim(c(0, 100)) +
  ylim(c(0, 50000)) +
  labs(title = "Number of Crimes and Population under 16",
       x = "Population Proportion under 16", y = "Number of Crimes") 

# there could be a slight correlation between higher proportion of under 16 residents meaning
# lower number of crimes
# again, Westminster seems to be an outlier


# number of crimes and population proportion in working age
train %>% 
  group_by(borough) %>%
  summarize(crimes = sum(value), population_working_age = unique(population_working_age)) %>%
  ggplot(aes(population_working_age, crimes, color = borough)) +
  geom_point(size = 4) +
  theme_classic() +
  geom_text_repel(aes(label = borough), size = 3.5, hjust = -0.2) +
  theme(legend.position = "none") + 
  xlim(c(0, 100)) +
  ylim(c(0, 50000)) +
  labs(title = "Number of Crimes and Population in Working Age",
       x = "Population Proportion in Working Age", y = "Number of Crimes") 


# number of crimes and population proportion over 64
train %>% 
  group_by(borough) %>%
  summarize(crimes = sum(value), population_over_64 = unique(population_over_64)) %>%
  ggplot(aes(population_over_64, crimes, color = borough)) +
  geom_point(size = 4) +
  theme_classic() +
  geom_text_repel(aes(label = borough), size = 3.5, hjust = -0.2) +
  xlim(c(0, 100)) +
  ylim(c(0, 50000)) +
  theme(legend.position = "none") + 
  labs(title = "Number of Crimes and Population over 64",
       x = "Population Proportion over 64", y = "Number of Crimes") 


# overall, the population age seems to have only little effect on the total number of crimes

# correlation coefficient for crimes vs. age variables
train %>% 
  group_by(borough) %>%
  summarize(crimes = sum(value), avg_age = unique(avg_age), 
            population_under_16 = unique(population_under_16), 
            population_working_age = unique(population_working_age), 
            population_over_64 = unique(population_over_64)) %>%
  summarize(cor(crimes, avg_age), cor(crimes, population_under_16), 
            cor(crimes, population_working_age), cor(crimes, population_over_64))

# medium to weak correlation between crimes and the four different age variables
# three correlation coefficients are negative and one is positive
# the correlation between crimes and average age seems to be the strongest



### employment and crimes

# employment rate per borough
train %>% 
  group_by(borough) %>%
  summarize(employment_rate_2015 = unique(employment_rate_2015)) %>%
  ggplot(aes(reorder(borough, -employment_rate_2015), employment_rate_2015)) +
  geom_bar(stat = "identity", fill = "steelblue4") +
  ylim(c(0, 100)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Employment Rate per Borough",
       x = "", y = "Employment Rate")


# number of crimes and employment rate
train %>% 
  group_by(borough) %>%
  summarize(crimes = sum(value), employment_rate_2015 = unique(employment_rate_2015)) %>%
  ggplot(aes(employment_rate_2015, crimes, color = borough)) +
  geom_point(size = 4) +
  theme_classic() +
  geom_text_repel(aes(label = borough), size = 3.5, hjust = -0.2) +
  xlim(c(0, 100)) +
  ylim(c(0, 50000)) +
  theme(legend.position = "none") + 
  labs(title = "Number of Crimes and Employment Rate",
       x = "Employment Rate", y = "Number of Crimes") 


# number of crimes and male employment rate
train %>% 
  group_by(borough) %>%
  summarize(crimes = sum(value), employment_rate_male_2015 = unique(employment_rate_male_2015)) %>%
  ggplot(aes(employment_rate_male_2015, crimes, color = borough)) +
  geom_point(size = 4) +
  theme_classic() +
  geom_text_repel(aes(label = borough), size = 3.5, hjust = -0.2) +
  xlim(c(0, 100)) +
  ylim(c(0, 50000)) +
  theme(legend.position = "none") + 
  labs(title = "Number of Crimes and Male Employment Rate",
       x = "Male Employment Rate", y = "Number of Crimes") 


# number of crimes and female employment rate
train %>% 
  group_by(borough) %>%
  summarize(crimes = sum(value), employment_rate_female_2015 = unique(employment_rate_female_2015)) %>%
  ggplot(aes(employment_rate_female_2015, crimes, color = borough)) +
  geom_point(size = 4) +
  theme_classic() +
  geom_text_repel(aes(label = borough), size = 3.5, hjust = -0.2) +
  xlim(c(0, 100)) +
  ylim(c(0, 50000)) +
  theme(legend.position = "none") + 
  labs(title = "Number of Crimes and Female Employment Rate",
       x = "Female Employment Rate", y = "Number of Crimes") 

# there seems to be somewhat of a negative correlation between crimes and male employment rate
# female employment rate seems to have a much weaker effect than male employment rate

# number of crimes and unemployment rate
train %>% 
  group_by(borough) %>%
  summarize(crimes = sum(value), unemployment_rate_2015 = unique(unemployment_rate_2015)) %>%
  ggplot(aes(unemployment_rate_2015, crimes, color = borough)) +
  geom_point(size = 4) +
  theme_classic() +
  geom_text_repel(aes(label = borough), size = 3.5, hjust = -0.2) +
  xlim(c(0, 100)) +
  ylim(c(0, 50000)) +
  theme(legend.position = "none") + 
  labs(title = "Number of Crimes and Unemployment Rate",
       x = "Unemployment Rate", y = "Number of Crimes") 


# correlation coefficient for crimes vs. employment variables
train %>% 
  group_by(borough) %>%
  filter(borough != "City of London") %>%
  summarize(crimes = sum(value), employment_rate_2015 = unique(employment_rate_2015), 
            employment_rate_male_2015 = unique(employment_rate_male_2015), 
            employment_rate_female_2015 = unique(employment_rate_female_2015), 
            unemployment_rate_2015 = unique(unemployment_rate_2015)) %>%
  summarize(cor(crimes, employment_rate_2015), cor(crimes, employment_rate_male_2015), 
            cor(crimes, employment_rate_female_2015), cor(crimes, unemployment_rate_2015))

# for the borough City of London there is no employment rate female and no unemployment
# rate given, so we need to filter this (otherwise the correlation coefficient = NA)

# the correlation coefficient shows the strongest correlation between crimes and employment rate 
# male
# as employment rate is the aggregation of employment rate male and female and the latter
# showing only a weak correlation, is would make most sense to include employment rate male in
# the prediction model
# the unemployment rate again is just a slightly different measure for the same issue, so we'll
# only use the most promising of these variables


# remove the temporary datasets that are not needed anymore
rm(crime_and_population, crimes_per_day)


# Results of the EDA:
# the most promision factors to include in our prediction model seem to be months, households, 
# population density as well as average age and male employment rate





#################################################################################################
# Prediction Model 
#################################################################################################

# set the seed for some of the predictions in this section
set.seed(1991)

################################################
# Metric to Evaluate the Model: RMSE
################################################

# why I chose RMSE:
# popular metrics to evaluate model performance include
#  - R-Squared (R²)
#  - Adjusted R-Squared (Adj R²)
#  - Mean Square Errors (MSE)
#  - Root Mean Squared Errors (RMSE) 
# RMSE comes in the same unit as the predicted variable, which makes it relatively easy to 
# interpret. It measures how accurate the prediction is. Low values of RMSE mean that the 
# predicted regression line is close to the data points.

# more info about the metrics:
# https://towardsdatascience.com/predictive-modellers-guide-to-choosing-the-best-fit-regression-model-707120e502b4


# define the loss function (RMSE)
RMSE <- function(reported_crimes, predicted_crimes){
  sqrt(mean((reported_crimes - predicted_crimes)^2))
}


################################################
# Base Prediction: Average
################################################

# auxiliary table with boroughs and months to predict
match_table <- test %>%
  group_by(month, borough) %>%
  summarize(crimes = sum(value)) %>%
  select(month, borough)

match_table

# average crimes per month as prediction
avg_crimes <- train %>%
  group_by(month) %>%
  summarize(avg_crimes = sum(value) / n_distinct(borough))

avg_crimes

# join the match table and the average crimes per borough to get predicted values into a table
mu_temp <- left_join(match_table, avg_crimes, by = "month")
mu_temp

# leave only the avg crimes in table to evaluate against
mu <- mu_temp %>%
  .$avg_crimes
mu

# calculate reported crimes per borough and month in test set
reported_crimes_temp <- test %>%
  group_by(month, borough) %>%
  summarise(crimes = sum(value))

reported_crimes_temp

reported_crimes <- reported_crimes_temp %>%
  .$crimes

reported_crimes

# evaluate the prediction with the test set
rmse_naive <- RMSE(reported_crimes, mu)
rmse_naive

# store the prediction results in a table
rmse_results <- tibble(Method = "Average Crimes per Borough", RMSE = rmse_naive)
rmse_results

# RMSE interpretation (the RMSE comes in the same unit as the predicted variable): 
# we are missing by over 700 crimes per month 

# checking additional measures of forecast accuracy just for fun
round(accuracy(mu, reported_crimes), 0)



################################################
# Linear Regression
################################################

# put together the reduced dataset only including the variables needed for linear regression
train_red <- train %>%
  group_by(month, borough) %>%
  summarize(crimes = sum(value), household = unique(household), 
            population_density = unique(population_density), 
            avg_age = unique(avg_age),
            employment_rate_male_2015 = unique(employment_rate_male_2015)) %>%
  select(month, borough, crimes, household, population_density, 
         avg_age, employment_rate_male_2015)

test_red <- test %>%
  group_by(month, borough) %>%
  summarize(crimes = sum(value), household = unique(household), 
            population_density = unique(population_density), 
            avg_age = unique(avg_age),
            employment_rate_male_2015 = unique(employment_rate_male_2015)) %>%
  select(month, borough, crimes, household, population_density, 
         avg_age, employment_rate_male_2015)

# reorder reported crimes so we are matching the correct month and borough for the evaluation of
# the linear regression models
reported_crimes_temp <- test %>%
  group_by(month, borough) %>%
  summarise(crimes = sum(value))

reported_crimes_temp

reported_crimes <- reported_crimes_temp %>%
  .$crimes

reported_crimes


# fit regression line to predict crimes
fit_lm0 <- lm(crimes ~ month, data = train_red)

# check out the statistics of the fit
tidy(fit_lm0, conf.int = TRUE)

# predict crimes for the test data with the fitted regression
predicted_crimes_lm0 <- predict(fit_lm0, newdata = test_red)

# evaluate the prediction with the test set
rmse_lm0 <- RMSE(reported_crimes, predicted_crimes_lm0)
rmse_lm0

# lm prediction with month only is almost the same as just the simple average


# predict crimes for the other possible lm models
fit_lm1 <- lm(crimes ~ month + household, data = train_red)
fit_lm2 <- lm(crimes ~ month + household + population_density, data = train_red)
fit_lm3 <- lm(crimes ~ month + household + population_density + avg_age, data = train_red)
fit_lm4 <- lm(crimes ~ month + household + population_density + avg_age +
            employment_rate_male_2015, data = train_red)

predicted_crimes_lm1 <- predict(fit_lm1, newdata = test_red)
predicted_crimes_lm2 <- predict(fit_lm2, newdata = test_red)
predicted_crimes_lm3 <- predict(fit_lm3, newdata = test_red)
predicted_crimes_lm4 <- predict(fit_lm4, newdata = test_red)

rmse_lm1 <- RMSE(reported_crimes, predicted_crimes_lm1)
rmse_lm2 <- RMSE(reported_crimes, predicted_crimes_lm2)
rmse_lm3 <- RMSE(reported_crimes, predicted_crimes_lm3)
rmse_lm4 <- RMSE(reported_crimes, predicted_crimes_lm4)

# store the prediction results in a table
rmse_results <- bind_rows(rmse_results,
                          tibble(Method = c("LM month", 
                                            "LM month+households", 
                                            "LM month+households+density", 
                                            "LM month+households+density+age",
                                            "LM month+households+density+age+employment"), 
                                 RMSE = c(rmse_lm0, rmse_lm1, rmse_lm2, rmse_lm3, rmse_lm4)))
rmse_results

# the model with month, households and population density performs best


# check out the other measures for forecast accuracy just for fun
round(accuracy(mu, reported_crimes), 0)
round(accuracy(predicted_crimes_lm0, reported_crimes), 0)
round(accuracy(predicted_crimes_lm1, reported_crimes), 0)
round(accuracy(predicted_crimes_lm2, reported_crimes), 0)
round(accuracy(predicted_crimes_lm3, reported_crimes), 0)
round(accuracy(predicted_crimes_lm4, reported_crimes), 0)



# charts comparing reported crimes and predictions for the different lm models

# get the month column out of the test_red dataset and add it to predicted crimes
# in order to add the predictions to the chart later on
month_col <- test_red %>%
  .$month 
borough_col <- test_red %>%
  .$borough

predicted_crimes_lm0 <- as_tibble(predicted_crimes_lm0)
predicted_crimes_lm0 <- predicted_crimes_lm0 %>%
  mutate(month = month_col, borough = borough_col, crimes = value)

predicted_crimes_lm1 <- as_tibble(predicted_crimes_lm1)
predicted_crimes_lm1 <- predicted_crimes_lm1 %>%
  mutate(month = month_col, borough = borough_col, crimes = value)

predicted_crimes_lm2 <- as_tibble(predicted_crimes_lm2)
predicted_crimes_lm2 <- predicted_crimes_lm2 %>%
  mutate(month = month_col, borough = borough_col, crimes = value)

predicted_crimes_lm3 <- as_tibble(predicted_crimes_lm3)
predicted_crimes_lm3 <- predicted_crimes_lm3 %>%
  mutate(month = month_col, borough = borough_col, crimes = value)

predicted_crimes_lm4 <- as_tibble(predicted_crimes_lm4)
predicted_crimes_lm4 <- predicted_crimes_lm4 %>%
  mutate(month = month_col, borough = borough_col, crimes = value)


# faceted line chart with all boroughs and prediction 0
test_red %>%
  group_by(month, borough) %>%
  ggplot(aes(month, crimes, color = borough)) +
  geom_line(show.legend = FALSE, size = 1) +
  ylim(0, 3400) +
  scale_x_continuous(breaks = c(1:12)) +
  geom_line(data = predicted_crimes_lm0, color = "darkgrey", show.legend = FALSE) +
  facet_wrap(~borough) +
  theme_minimal() +
  labs(title = "Reported Crimes vs. LM Prediction 0",
       x = "Month", y = "Number of Crimes")

# faceted line chart with all boroughs and prediction 1
test_red %>%
  group_by(month, borough) %>%
  ggplot(aes(month, crimes, color = borough)) +
  geom_line(show.legend = FALSE, size = 1) +
  ylim(0, 3400) +
  scale_x_continuous(breaks = c(1:12)) +
  geom_line(data = predicted_crimes_lm1, color = "darkgrey", show.legend = FALSE) +
  facet_wrap(~borough) +
  theme_minimal() +
  geom_line(show.legend = FALSE, size = 1) +
  labs(title = "Reported Crimes vs. LM Prediction 1",
       x = "Month", y = "Number of Crimes")


# faceted line chart with all boroughs and prediction 2
test_red %>%
  group_by(month, borough) %>%
  ggplot(aes(month, crimes, color = borough)) +
  geom_line(show.legend = FALSE, size = 1) +
  ylim(0, 3400) +
  scale_x_continuous(breaks = c(1:12)) +
  geom_line(data = predicted_crimes_lm2, color = "darkgrey", show.legend = FALSE) +
  facet_wrap(~borough) +
  theme_minimal() +
  labs(title = "Reported Crimes vs. LM Prediction 2",
       x = "Month", y = "Number of Crimes")


# faceted line chart with all boroughs and prediction 3
test_red %>%
  group_by(month, borough) %>%
  ggplot(aes(month, crimes, color = borough)) +
  geom_line(show.legend = FALSE, size = 1) +
  ylim(0, 3400) +
  scale_x_continuous(breaks = c(1:12)) +
  geom_line(data = predicted_crimes_lm3, color = "darkgrey", show.legend = FALSE) +
  facet_wrap(~borough) +
  theme_minimal() +
  labs(title = "Reported Crimes vs. LM Prediction 3",
       x = "Month", y = "Number of Crimes")


# faceted line chart with all boroughs and prediction 4
test_red %>%
  group_by(month, borough) %>%
  ggplot(aes(month, crimes, color = borough)) +
  geom_line(show.legend = FALSE, size = 1) +
  ylim(0, 3400) +
  scale_x_continuous(breaks = c(1:12)) +
  geom_line(data = predicted_crimes_lm4, color = "darkgrey", show.legend = FALSE) +
  facet_wrap(~borough) +
  theme_minimal() +
  labs(title = "Reported Crimes vs. LM Prediction 4",
       x = "Month", y = "Number of Crimes")


# table with summary of variables for test set -> check out why we are off in which prediction
test_red %>%
  group_by(borough) %>%
  summarize("AVG Crimes per Month" = round(sum(crimes) / n_distinct(month)),
            "Households" = unique(household),
            "Population Density" = unique(population_density),
            "Average Age" = unique(avg_age),
            "Employment Rate Male" = unique(employment_rate_male_2015))




################################################
# K-Nearest Neighbours Algorithm
################################################

# get info about the function/model
getModelInfo("knn")

# Course 8 Machine Learning on the train function with method = knn: 
# By default, the cross-validation is performed by testing on 25 bootstrap samples comprised 
# of 25% of the observations. Also, for the knn method, the default is to try out k=5, 7, and 9.

# check the dimensions of train_red to figure out which range for k would be possible
dim(train_red)

# we don't want to use all neighbours for prediction, as that would include very far away 
# neighbours, so we are trying a sequence up to 100 for tuning

# Course 8 Machine Learning:
# Over-training is the reason that we have higher accuracy in the train set compared to the test
# set. Over-training is at its worst when we set k=1. With k=1, the estimate for each (x1,x2)
# in the training set is obtained with just the y corresponding to that point. 
# When we try a larger k, the k might be so large that it does not permit enough flexibility. 
# We call this over-smoothing.

# fit knn to predict crimes and simultaneously tune k
fit_knn0 <- train(crimes ~ month, method = "knn", data = train_red,
                  tuneGrid = data.frame(k = seq(1, 100, 1)))

# check which k is the best tune
fit_knn0$bestTune
fit_knn0$finalModel

ggplot(fit_knn0, highlight = TRUE)     

# predict crimes for the test data
predicted_crimes_knn0 <- predict(fit_knn0, test_red)

# evaluate the prediction 
rmse_knn0 <- RMSE(reported_crimes, predicted_crimes_knn0)
rmse_knn0


# predict crimes for the other possible knn models
fit_knn1 <- train(crimes ~ month + household, 
                  method = "knn", data = train_red, tuneGrid = data.frame(k = seq(1, 100, 1)))
fit_knn2 <- train(crimes ~ month + household + population_density, 
                  method = "knn", data = train_red, tuneGrid = data.frame(k = seq(1, 100, 1)))
fit_knn3 <- train(crimes ~ month + household + population_density + avg_age, 
                  method = "knn", data = train_red, tuneGrid = data.frame(k = seq(1, 100, 1)))
fit_knn4 <- train(crimes ~ month + household + population_density + avg_age + 
                    employment_rate_male_2015, 
                  method = "knn", data = train_red, tuneGrid = data.frame(k = seq(1, 100, 1)))

predicted_crimes_knn1 <- predict(fit_knn1, newdata = test_red)
predicted_crimes_knn2 <- predict(fit_knn2, newdata = test_red)
predicted_crimes_knn3 <- predict(fit_knn3, newdata = test_red)
predicted_crimes_knn4 <- predict(fit_knn4, newdata = test_red)

rmse_knn1 <- RMSE(reported_crimes, predicted_crimes_knn1)
rmse_knn2 <- RMSE(reported_crimes, predicted_crimes_knn2)
rmse_knn3 <- RMSE(reported_crimes, predicted_crimes_knn3)
rmse_knn4 <- RMSE(reported_crimes, predicted_crimes_knn4)

# store the prediction results in a table
rmse_results <- bind_rows(rmse_results,
                          tibble(Method = c("KNN month", 
                                            "KNN month+households", 
                                            "KNN month+households+density", 
                                            "KNN month+households+density+age",
                                            "KNN month+households+density+age+employment"), 
                                 RMSE = c(rmse_knn0, rmse_knn1, rmse_knn2, rmse_knn3, rmse_knn4)))
rmse_results

# the knn models with more than just month as regressor perform relatively well compared to our 
# starting point of the overall average, but the lm model with month, households and population
# density is still by far the best model we have tried so far


# check out the other measures for forecast accuracy just for fun
round(accuracy(mu, reported_crimes), 0)
round(accuracy(predicted_crimes_knn0, reported_crimes), 0)
round(accuracy(predicted_crimes_knn1, reported_crimes), 0)
round(accuracy(predicted_crimes_knn2, reported_crimes), 0)
round(accuracy(predicted_crimes_knn3, reported_crimes), 0)
round(accuracy(predicted_crimes_knn4, reported_crimes), 0)


# charts comparing reported crimes and predictions for the different knn models

predicted_crimes_knn0 <- as_tibble(predicted_crimes_knn0)
predicted_crimes_knn0 <- predicted_crimes_knn0 %>%
  mutate(month = month_col, borough = borough_col, crimes = value)

predicted_crimes_knn1 <- as_tibble(predicted_crimes_knn1)
predicted_crimes_knn1 <- predicted_crimes_knn1 %>%
  mutate(month = month_col, borough = borough_col, crimes = value)

predicted_crimes_knn2 <- as_tibble(predicted_crimes_knn2)
predicted_crimes_knn2 <- predicted_crimes_knn2 %>%
  mutate(month = month_col, borough = borough_col, crimes = value)

predicted_crimes_knn3 <- as_tibble(predicted_crimes_knn3)
predicted_crimes_knn3 <- predicted_crimes_knn3 %>%
  mutate(month = month_col, borough = borough_col, crimes = value)

predicted_crimes_knn4 <- as_tibble(predicted_crimes_knn4)
predicted_crimes_knn4 <- predicted_crimes_knn4 %>%
  mutate(month = month_col, borough = borough_col, crimes = value)


# faceted line chart with all boroughs and prediction 0
test_red %>%
  group_by(month, borough) %>%
  ggplot(aes(month, crimes, color = borough)) +
  geom_line(show.legend = FALSE, size = 1) +
  ylim(0, 3400) +
  scale_x_continuous(breaks = c(1:12)) +
  geom_line(data = predicted_crimes_knn0, color = "darkgrey", show.legend = FALSE) +
  facet_wrap(~borough) +
  theme_minimal() +
  labs(title = "Reported Crimes vs. KNN Prediction 0",
       x = "Month", y = "Number of Crimes")

# faceted line chart with all boroughs and prediction 1
test_red %>%
  group_by(month, borough) %>%
  ggplot(aes(month, crimes, color = borough)) +
  geom_line(show.legend = FALSE, size = 1) +
  ylim(0, 3400) +
  scale_x_continuous(breaks = c(1:12)) +
  geom_line(data = predicted_crimes_knn1, color = "darkgrey", show.legend = FALSE) +
  facet_wrap(~borough) +
  theme_minimal() +
  geom_line(show.legend = FALSE, size = 1) +
  labs(title = "Reported Crimes vs. KNN Prediction 1",
       x = "Month", y = "Number of Crimes")


# faceted line chart with all boroughs and prediction 2
test_red %>%
  group_by(month, borough) %>%
  ggplot(aes(month, crimes, color = borough)) +
  geom_line(show.legend = FALSE, size = 1) +
  ylim(0, 3400) +
  scale_x_continuous(breaks = c(1:12)) +
  geom_line(data = predicted_crimes_knn2, color = "darkgrey", show.legend = FALSE) +
  facet_wrap(~borough) +
  theme_minimal() +
  labs(title = "Reported Crimes vs. KNN Prediction 2",
       x = "Month", y = "Number of Crimes")


# faceted line chart with all boroughs and prediction 3
test_red %>%
  group_by(month, borough) %>%
  ggplot(aes(month, crimes, color = borough)) +
  geom_line(show.legend = FALSE, size = 1) +
  ylim(0, 3400) +
  scale_x_continuous(breaks = c(1:12)) +
  geom_line(data = predicted_crimes_knn3, color = "darkgrey", show.legend = FALSE) +
  facet_wrap(~borough) +
  theme_minimal() +
  labs(title = "Reported Crimes vs. KNN Prediction 3",
       x = "Month", y = "Number of Crimes")

# faceted line chart with all boroughs and prediction 4
test_red %>%
  group_by(month, borough) %>%
  ggplot(aes(month, crimes, color = borough)) +
  geom_line(show.legend = FALSE, size = 1) +
  ylim(0, 3400) +
  scale_x_continuous(breaks = c(1:12)) +
  geom_line(data = predicted_crimes_knn4, color = "darkgrey", show.legend = FALSE) +
  facet_wrap(~borough) +
  theme_minimal() +
  labs(title = "Reported Crimes vs. KNN Prediction 4",
       x = "Month", y = "Number of Crimes")





################################################
# Regression Tree (rpart)
################################################

# Course 8 Machine Learning:
# To fit the regression tree model, we can use the rpart function in the rpart package.
# Two common parameters used for partition decision are the complexity parameter (cp) and the 
# minimum number of observations required in a partition before partitioning it further (minsplit 
# in the rpart package). 

# cp: “minimum benefit” that a split must add to the tree
# cp = 0 -> most complex tree possible


# check out the complexity parameter and how extreme values change the prediction outcome
# to get a feeling for this parameter
#
# if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
# library(rpart.plot)
# 
# # try different values for cp here (e.g. 0 to 0.2)
# fit_rpart <- rpart(crimes ~ month + household + population_density, data = train_red, cp = 0.1)
# 
# rpart.plot(fit_rpart)
# 
# predicted_crimes_rpart <- predict(fit_rpart, newdata = test_red)
# 
# rmse_rpart <- RMSE(reported_crimes, predicted_crimes_rpart)
# rmse_rpart


# fit the rpart model and simultaneously use cross validation to choose the best cp
# (as our dataset is relatively small, we can allow very complex models with cp = 0)
fit_rpart0 <- train(crimes ~ month, 
                    method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.10, len = 50)), 
                    data = train_red)

# tuning: look at the RMSE for the different cp
ggplot(fit_rpart0, highlight = TRUE)

# predict crimes in the test set
predicted_crimes_rpart0 <- predict(fit_rpart0, newdata = test_red)

# evaluate prediction for the test set
rmse_rpart0 <- RMSE(reported_crimes, predicted_crimes_rpart0)


# predict crimes for the other possible rpart models
fit_rpart1 <- train(crimes ~ month + household, 
                    method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.10, len = 50)), 
                    data = train_red)
fit_rpart2 <- train(crimes ~ month + household + population_density, 
                    method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.10, len = 50)), 
                    data = train_red)
fit_rpart3 <- train(crimes ~ month + household + population_density + avg_age, 
                    method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.10, len = 50)), 
                    data = train_red)
fit_rpart4 <- train(crimes ~ month + household + population_density + avg_age +
                      employment_rate_male_2015, 
                    method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.10, len = 50)), 
                    data = train_red)

predicted_crimes_rpart1 <- predict(fit_rpart1, newdata = test_red)
predicted_crimes_rpart2 <- predict(fit_rpart2, newdata = test_red)
predicted_crimes_rpart3 <- predict(fit_rpart3, newdata = test_red)
predicted_crimes_rpart4 <- predict(fit_rpart4, newdata = test_red)

rmse_rpart1 <- RMSE(reported_crimes, predicted_crimes_rpart1)
rmse_rpart2 <- RMSE(reported_crimes, predicted_crimes_rpart2)
rmse_rpart3 <- RMSE(reported_crimes, predicted_crimes_rpart3)
rmse_rpart4 <- RMSE(reported_crimes, predicted_crimes_rpart4)

# store the prediction results in a table
rmse_results <- bind_rows(rmse_results,
                          tibble(Method = c("RPart month", 
                                            "RPart month+households", 
                                            "RPart month+households+density", 
                                            "RPart month+households+density+age",
                                            "RPart month+households+density+age+employment"), 
                                 RMSE = c(rmse_rpart0, rmse_rpart1, rmse_rpart2, rmse_rpart3, 
                                          rmse_rpart4)))
rmse_results



# due to the very good rmse results of models rpart3 and rpart5 we try another model to check
# whether we can further improve the results with this combination
fit_rpart5 <- train(crimes ~ month + household + population_density + employment_rate_male_2015, 
                    method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.10, len = 50)), 
                    data = train_red)
predicted_crimes_rpart5 <- predict(fit_rpart5, newdata = test_red)

rmse_rpart5 <- RMSE(reported_crimes, predicted_crimes_rpart5)
rmse_rpart5

# store the prediction results in a table
rmse_results <- bind_rows(rmse_results,
                          tibble(Method = c("RPart month+households+density+employment"), 
                                 RMSE = c(rmse_rpart5)))
rmse_results

# unfortunately the combined model performs worse than the single ones



# charts comparing reported crimes and predictions for the different rpart models

predicted_crimes_rpart0 <- as_tibble(predicted_crimes_rpart0)
predicted_crimes_rpart0 <- predicted_crimes_rpart0 %>%
  mutate(month = month_col, borough = borough_col, crimes = value)

predicted_crimes_rpart1 <- as_tibble(predicted_crimes_rpart1)
predicted_crimes_rpart1 <- predicted_crimes_rpart1 %>%
  mutate(month = month_col, borough = borough_col, crimes = value)

predicted_crimes_rpart2 <- as_tibble(predicted_crimes_rpart2)
predicted_crimes_rpart2 <- predicted_crimes_rpart2 %>%
  mutate(month = month_col, borough = borough_col, crimes = value)

predicted_crimes_rpart3 <- as_tibble(predicted_crimes_rpart3)
predicted_crimes_rpart3 <- predicted_crimes_rpart3 %>%
  mutate(month = month_col, borough = borough_col, crimes = value)

predicted_crimes_rpart4 <- as_tibble(predicted_crimes_rpart4)
predicted_crimes_rpart4 <- predicted_crimes_rpart4 %>%
  mutate(month = month_col, borough = borough_col, crimes = value)

predicted_crimes_rpart5 <- as_tibble(predicted_crimes_rpart5)
predicted_crimes_rpart5 <- predicted_crimes_rpart5 %>%
  mutate(month = month_col, borough = borough_col, crimes = value)


# faceted line chart with all boroughs and prediction 0
test_red %>%
  group_by(month, borough) %>%
  ggplot(aes(month, crimes, color = borough)) +
  geom_line(show.legend = FALSE, size = 1) +
  ylim(0, 3400) +
  scale_x_continuous(breaks = c(1:12)) +
  geom_line(data = predicted_crimes_rpart0, color = "darkgrey", show.legend = FALSE) +
  facet_wrap(~borough) +
  theme_minimal() +
  labs(title = "Reported Crimes vs. RPart Prediction 0",
       x = "Month", y = "Number of Crimes")

# faceted line chart with all boroughs and prediction 1
test_red %>%
  group_by(month, borough) %>%
  ggplot(aes(month, crimes, color = borough)) +
  geom_line(show.legend = FALSE, size = 1) +
  ylim(0, 3400) +
  scale_x_continuous(breaks = c(1:12)) +
  geom_line(data = predicted_crimes_rpart1, color = "darkgrey", show.legend = FALSE) +
  facet_wrap(~borough) +
  theme_minimal() +
  geom_line(show.legend = FALSE, size = 1) +
  labs(title = "Reported Crimes vs. RPart Prediction 1",
       x = "Month", y = "Number of Crimes")


# faceted line chart with all boroughs and prediction 2
test_red %>%
  group_by(month, borough) %>%
  ggplot(aes(month, crimes, color = borough)) +
  geom_line(show.legend = FALSE, size = 1) +
  ylim(0, 3400) +
  scale_x_continuous(breaks = c(1:12)) +
  geom_line(data = predicted_crimes_rpart2, color = "darkgrey", show.legend = FALSE) +
  facet_wrap(~borough) +
  theme_minimal() +
  labs(title = "Reported Crimes vs. RPart Prediction 2",
       x = "Month", y = "Number of Crimes")


# faceted line chart with all boroughs and prediction 3 (larger y axis for this chart!)
test_red %>%
  group_by(month, borough) %>%
  ggplot(aes(month, crimes, color = borough)) +
  geom_line(show.legend = FALSE, size = 1) +
  ylim(0, 5000) +
  scale_x_continuous(breaks = c(1:12)) +
  geom_line(data = predicted_crimes_rpart3, color = "darkgrey", show.legend = FALSE) +
  facet_wrap(~borough) +
  theme_minimal() +
  labs(title = "Reported Crimes vs. RPart Prediction 3",
       x = "Month", y = "Number of Crimes")


# faceted line chart with all boroughs and prediction 4
test_red %>%
  group_by(month, borough) %>%
  ggplot(aes(month, crimes, color = borough)) +
  geom_line(show.legend = FALSE, size = 1) +
  ylim(0, 3400) +
  scale_x_continuous(breaks = c(1:12)) +
  geom_line(data = predicted_crimes_rpart4, color = "darkgrey", show.legend = FALSE) +
  facet_wrap(~borough) +
  theme_minimal() +
  labs(title = "Reported Crimes vs. RPart Prediction 4",
       x = "Month", y = "Number of Crimes")


# faceted line chart with all boroughs and prediction 5
test_red %>%
  group_by(month, borough) %>%
  ggplot(aes(month, crimes, color = borough)) +
  geom_line(show.legend = FALSE, size = 1) +
  ylim(0, 3400) +
  scale_x_continuous(breaks = c(1:12)) +
  geom_line(data = predicted_crimes_rpart5, color = "darkgrey", show.legend = FALSE) +
  facet_wrap(~borough) +
  theme_minimal() +
  labs(title = "Reported Crimes vs. RPart Prediction 5",
       x = "Month", y = "Number of Crimes")



### Results of the Model Development

# the best performing model is RPart Month + Households + Density
rmse_results$Method[which.min(rmse_results$RMSE)]
min(rmse_results$RMSE)

# chart comparing the RMSE for the different models on the test set
rmse_results %>%
  ggplot(aes(Method, RMSE, fill = ifelse(Method == "RPart month+households+density", "A", "B"))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = c(A = "steelblue2", B = "grey95")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "RMSE Results for the Different Models",
       x = "RMSE", y = "Method")


# other well performing models:
# RPart month+households+density+age+employment
# LM month+households+density



#################################################################################################
# Results 
#################################################################################################

# first of all we are going create a dataset combining the train and test sets so we have more
# data to train our final model on in the hope of improving prediction results
# this does not represent overtraining (see lecture and forum comments from staff)
# we have not used the validation dataset at all until now
# we are just adding together the train and test sets and will train our final model on this 
# enlarged dataset again to give it as much training data as possible

train_red_enlarged <- full_join(train_red, test_red) 

head(train_red_enlarged)
tail(train_red_enlarged)
unique(train_red_enlarged$borough)
n_distinct(train_red_enlarged$borough)

# so we now have 27 boroughs to train our model on instead of 21


################################################
# Evaluate Results on the Validation Dataset
################################################

head(validation)

# before we can do the predictions, we need to do some data preparation on the validation set

# get the validation dataset into the same reduced structure as the train_red and test_red sets
validation_red <- validation %>%
  group_by(month, borough) %>%
  summarize(crimes = sum(value), household = unique(household), 
            population_density = unique(population_density), 
            avg_age = unique(avg_age),
            employment_rate_male_2015 = unique(employment_rate_male_2015)) %>%
  select(month, borough, crimes, household, population_density, 
         avg_age, employment_rate_male_2015)

head(validation_red)

# put together the reported crimes vector to evaluate against based on the validation dataset
reported_crimes_val <- validation %>%
  group_by(month, borough) %>%
  summarise(crimes = sum(value))

reported_crimes_val <- reported_crimes_val %>%
  .$crimes

reported_crimes_val

# get the auxiliary month and borough columns
month_col <- validation_red %>%
  .$month 
borough_col <- validation_red %>%
  .$borough



# now we can do the predictions

# fit the rpart model 
# (using the rpart function instead of the train function, as we don't want to do any tuning, 
# which the train function always includes, but use the best tune from rpart3)
fit_val_rpart3 <- rpart(crimes ~ month + household + population_density, 
                        data = train_red_enlarged, 
                        control = rpart.control(cp = fit_rpart3$bestTune))

# predict crimes in the validation set
predicted_crimes_val_rpart3 <- predict(fit_val_rpart3, newdata = validation_red)

# evaluate prediction for the validation set
rmse_val_rpart3 <- RMSE(reported_crimes_val, predicted_crimes_val_rpart3)
rmse_val_rpart3


# preparation for the chart
predicted_crimes_val_rpart3 <- as_tibble(predicted_crimes_val_rpart3)
predicted_crimes_val_rpart3 <- predicted_crimes_val_rpart3 %>%
  mutate(month = month_col, borough = borough_col, crimes = value)


# faceted line chart with all boroughs and prediction for validation dataset
validation_red %>%
  group_by(month, borough) %>%
  ggplot(aes(month, crimes, color = borough)) +
  geom_line(show.legend = FALSE, size = 1) +
#  ylim(0, 3400) +
  scale_x_continuous(breaks = c(1:12)) +
  geom_line(data = predicted_crimes_val_rpart3, color = "darkgrey", show.legend = FALSE) +
  facet_wrap(~borough) +
  theme_minimal() +
  labs(title = "Reported Crimes vs. RPart Prediction 3 - Validation Dataset",
       x = "Month", y = "Number of Crimes")

# with almost 500 the RMSE for the prediction of the validation set is definitely not as good as 
# the prediction of the test set (however, this is normal, as we selected the best performing
# model for the test set)

# the chart shows that the prediction is especially bad for the City of London, which is somewhat
# of an outlier as it has only about 10-30 crimes per month

validation_red %>%
  filter(borough == "City of London")


# if we do the same prediction again but without City of London, the performance is much better
# this means, that we would need to build a seperate prediction for the City of London, to get
# all preditions needed (just as a thought, we are not doing that in this project)



# prediction without City of London

# same prediction, but without City of London
validation_red_noCoL <- validation %>%
  filter(borough != "City of London") %>%
  group_by(month, borough) %>%
  summarize(crimes = sum(value), household = unique(household), 
            population_density = unique(population_density), 
            avg_age = unique(avg_age),
            employment_rate_male_2015 = unique(employment_rate_male_2015)) %>%
  select(month, borough, crimes, household, population_density, 
         avg_age, employment_rate_male_2015)

head(validation_red_noCoL)

# put together the reported crimes vector to evaluate against based on the validation dataset
reported_crimes_val_noCoL <- validation %>%
  filter(borough != "City of London") %>%
  group_by(month, borough) %>%
  summarise(crimes = sum(value))

reported_crimes_val_noCoL <- reported_crimes_val_noCoL %>%
  .$crimes

reported_crimes_val_noCoL

# get the auxiliary month and borough columns
month_col <- validation_red_noCoL %>%
  .$month 
borough_col <- validation_red_noCoL %>%
  .$borough



# fit the rpart model
fit_val_rpart3_noCoL <- rpart(crimes ~ month + household + population_density, 
                              data = train_red_enlarged, 
                              control = rpart.control(cp = fit_rpart3$bestTune))

# predict crimes in the validation set
predicted_crimes_val_rpart3_noCoL <- predict(fit_val_rpart3_noCoL, newdata = validation_red_noCoL)

# evaluate prediction for the validation set
rmse_val_rpart3_noCoL <- RMSE(reported_crimes_val_noCoL, predicted_crimes_val_rpart3_noCoL)
rmse_val_rpart3_noCoL


# preparation for the chart
predicted_crimes_val_rpart3_noCoL <- as_tibble(predicted_crimes_val_rpart3_noCoL)
predicted_crimes_val_rpart3_noCoL <- predicted_crimes_val_rpart3_noCoL %>%
  mutate(month = month_col, borough = borough_col, crimes = value)


# faceted line chart with all boroughs and prediction for validation dataset
validation_red_noCoL %>%
  group_by(month, borough) %>%
  ggplot(aes(month, crimes, color = borough)) +
  geom_line(show.legend = FALSE, size = 1) +
  ylim(0, 3400) +
  scale_x_continuous(breaks = c(1:12)) +
  geom_line(data = predicted_crimes_val_rpart3_noCoL, color = "darkgrey", show.legend = FALSE) +
  facet_wrap(~borough) +
  theme_minimal() +
  labs(title = "Reported Crimes vs. RPart Prediction 3 - Validation Dataset",
       x = "Month", y = "Number of Crimes")




#################################################################################################
# Conclusion 
#################################################################################################

# See pdf report and rmd file.


#################################################################################################
# End of Project
#################################################################################################
