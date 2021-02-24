#PSYC 259 Homework 4 - Writing functions
#For full credit, answer at least 6/8 questions

#List names of students collaborating with: 

### SETUP: RUN THIS BEFORE STARTING ----------
library(tidyverse)
set.seed(1)
id <- rep("name", 30)
x <- runif(30, 0, 10)
y <- runif(30, 0, 10)
z <- runif(30, 0, 10)
ds <- tibble(id, x, y, z)
ds2 <- tibble(id, x, y, z)
### Question 1 ---------- 

#Vectors x, y, and z contain random numbers between 1 and 10. 
#Write a function called "limit_replace" that will replace values less than 2 or greater than 8 with NA
#Then, run the function on x and save the results to a new vector "x_replace" to show it worked

#
#install.packages("sqldf")
#library(sqldf)

#sqldf("SELECT * FROM ds2 WHERE x < 2")
#sqldf("SELECT * FROM ds2 WHERE x > 8")
#na_if(ds2, sqldf("SELECT * FROM ds2 WHERE x < 2"))
#?replace
#?mutate
#ds2$x = replace(ds2, sqldf("SELECT x FROM ds2 WHERE x < 2"), NA)
#ds2$x = mutate(ds2$x, sqldf("SELECT x FROM ds2 WHERE x < 2") = NA)

#ds2 %>% sqldf("SELECT x FROM ds2 WHERE x < 2") %>% mutate(x=NA)

#ds2$x %>% select(ds2$x < 2) %>% mutate(x=NA)
#ds2 %>% select(x > 2)
#?select
#limit_replace = function(input){
#na_if(input, (input[] > 2))
#na_if(filter_all(input, all_vars(. > 2)))
#na_if(input, filter_all(input, all_vars(. < 8)))
#?mutate
#mutate_all(input, all_vars(. > 2))
#}

limit_replace = function(v){
  ifelse(v < 2 | v > 8, NA, v)
}
x_replace = limit_replace(x)

### Question 2 ---------- 

#Make a new version of limit_replace that asks for arguments for a lower boundary and an upper boundary
#so that they can be customized (instead of being hard coded as 2 and 8)
#Run the function on vector y with boundaries 4 and 6, saving the results to a new vector "y_replace"

limit_replace_b = function(data, lowerb = 2, upperb = 8){
  ifelse(data < lowerb | data > upperb, NA, data)
}
y_replace_v1 = limit_replace_b(y)
y_replace = limit_replace_b(y, 4, 6)

### Question 3 ----------

#Write a function called "plus_minus_SD" that can take one of the vectors (x/y/z) as input
#and "num_of_SDs" as an input and returns the boundaries +/- num_of_SDs around the mean. 
#plus_minus_SD(x, 1) would give +/- 1SD around the mean of x, plus_minus_SD(y, 2) would give +/- 2SDs around the mean 
#Make num_of_SDs default to 1
#run the new function on x, y, and z with 1 SD

plus_minus_sd = function(dataArg, num_of_SDs = 1){
  dmean = mean(dataArg)
  dsd = (sd(dataArg))*num_of_SDs
  c((paste0 ("Values within ",num_of_SDs," std.dev.:")),(dmean-dsd), dmean, (dmean+dsd))
}
plus_minus_sd(x)
plus_minus_sd(y)
plus_minus_sd(z, 2)

### Question 4 ----------

#Write an another new version of limit_replace
#This time, make the upper and lower boundaries optional arguments
#If they are not given, use +/- 1 SD as the boundaries (from your plus_minus_SD function)
#Apply the function to each column in ds, and save the results to a new tibble called "ds_replace"

#limit_replace_c = function(dataA, lowerb = -(sd(dataA)), upperb = (sd(dataA)){

limit_replace_c = function(dataA, lowerb = 1, upperb = 1) {
  #if(lowerb == 1) {lowerb = (mean(dataA)-sd(dataA))}
  lowerb = ifelse(lowerb == 1, (mean(dataA)-sd(dataA)), lowerb)
  #if(upperb == 1) {lowerb = (mean(dataA)+sd(dataA))}
  upperb = ifelse(upperb == 1, (mean(dataA)+sd(dataA)), upperb)
  #print(c(lowerb, upperb))
  
  ifelse(dataA < lowerb | dataA > upperb, NA, dataA)
  
}

xtest = limit_replace_c(x)
ytest = limit_replace_c(y)
ztest = limit_replace_c(z)
dsreplace = tibble(id, xtest, ytest, ztest)

### Question 5 ----------

#Add a "stopifnot" command to your limit_replace function to make sure it only runs on numeric variables
#Try running it on a non-numeric input (like "id") to make sure it gives you an error

limit_replace_d = function(dataA, lowerb = 1, upperb = 1) {
  stopifnot(is.numeric(dataA))
  #if(lowerb == 1) {lowerb = (mean(dataA)-sd(dataA))}
  lowerb = ifelse(lowerb == 1, (mean(dataA)-sd(dataA)), lowerb)
  #if(upperb == 1) {lowerb = (mean(dataA)+sd(dataA))}
  upperb = ifelse(upperb == 1, (mean(dataA)+sd(dataA)), upperb)
  #print(c(lowerb, upperb))
  
  ifelse(dataA < lowerb | dataA > upperb, NA, dataA)
  
}

limit_replace_d(id)

### Question 6 ----------

#What other requirements on the input do you need to make the function work correctly?
#Add another stopifnot to enforce one more requirement

limit_replace_e = function(dataA, lowerb = 1, upperb = 1) {
  stopifnot(is.numeric(dataA))
  stopifnot(is.numeric(lowerb)&is.numeric(upperb))
  #if(lowerb == 1) {lowerb = (mean(dataA)-sd(dataA))}
  lowerb = ifelse(lowerb == 1, (mean(dataA)-sd(dataA)), lowerb)
  #if(upperb == 1) {lowerb = (mean(dataA)+sd(dataA))}
  upperb = ifelse(upperb == 1, (mean(dataA)+sd(dataA)), upperb)
  #print(c(lowerb, upperb))
  
  ifelse(dataA < lowerb | dataA > upperb, NA, dataA)
  
}

limit_replace_e(x, "space", "explosion")
limit_replace_e(y, 3.141592, "explosion")
limit_replace_e(z, "3.141592", 21)
limit_replace_e(x, 1.45, 3.148)

### Question 7 ----------

#Clear out your workspace and load the built-in diamonds dataset by running the lines below
#RUN THIS CODE
rm(list = ls())
library(tidyverse)
ds_diamonds <- diamonds

#Save your two functions to an external file (or files) 
#Then, load your functions from the external files(s)
#Next, run your limit_replace function on all of the numeric columns in the new data set
#and drop any rows with NA, saving it to a new tibble named "ds_trimmed"

source("func.R")
names(ds_diamonds)
ds_trimmed = ds_diamonds

#iteration testing...
#?map
#map(.x = ds_diamonds, .f = limit_replace(.x))
#?"~"
#?formula
#?drop_na

#?apply
#apply(ds_trimmed, MARGIN = 1, FUN=limit_replace)
#apply(ds_trimmed, MARGIN = 2, FUN=limit_replace)
#need to skip non-numeric columns...?

#iteration remains tricky to grasp
#going to go with kludge solution
ds_trimmed$carat = limit_replace(ds_diamonds$carat)
ds_trimmed$depth = limit_replace(ds_diamonds$depth)
ds_trimmed$table = limit_replace(ds_diamonds$table)
ds_trimmed$price = limit_replace(ds_diamonds$price)
ds_trimmed$x = limit_replace(ds_diamonds$x)
ds_trimmed$y = limit_replace(ds_diamonds$y)
ds_trimmed$z = limit_replace(ds_diamonds$z)
ds_trimmed = drop_na(ds_trimmed)

### Question 8 ----------

#The code below makes graphs of diamond price grouped by different variables
#Refactor it to make it more efficient using functions and/or iteration
#Don't worry about the order of the plots, just find a more efficient way to make all 6 plots
#Each cut (Premium/Ideal/Good) should have a plot with trimmed and untrimmed data
#The title of each plot should indicate which cut and whether it's all vs. trimmed data

diamondplot = function(dataArg, cutArg, isTrimmed = F){
  label = ifelse(isTrimmed, "trimmed", "all")
  dataArg %>% filter(cut == cutArg) %>% 
    ggplot(aes(x = clarity, y = price)) + 
    geom_boxplot() + 
    ggtitle(paste0(cutArg,", ",label)) + 
    theme_minimal()
}

diamondplot(ds_diamonds, "Premium")
diamondplot(ds_diamonds, "Ideal")
diamondplot(ds_diamonds, "Good")
diamondplot(ds_trimmed, "Premium", T)
diamondplot(ds_trimmed, "Ideal", T)
diamondplot(ds_trimmed, "Good", T)

#I feel like there should be some way of getting the function to tell from the data name whether it is trimmed or not, but...
#... I wish I remembered how to get map and apply to work...
#I think this works though.


ds_diamonds %>% filter(cut == "Premium") %>% 
  ggplot(aes(x = clarity, y = price)) + 
  geom_boxplot() + 
  ggtitle("Premium, all") + 
  theme_minimal()

ds_diamonds %>% filter(cut == "Ideal") %>% 
  ggplot(aes(x = clarity, y = price)) + 
  geom_boxplot() + 
  ggtitle("Ideal, all") +
  theme_minimal()

ds_diamonds %>% filter(cut == "Good") %>% 
  ggplot(aes(x = clarity, y = price)) + 
  geom_boxplot() + 
  ggtitle("Good, all") +
  theme_minimal()

ds_trimmed %>% filter(cut == "Premium") %>% 
  ggplot(aes(x = clarity, y = price)) + 
  geom_boxplot() + 
  ggtitle("Premium, trimmed") + 
  theme_minimal()

ds_trimmed %>% filter(cut == "Ideal") %>% 
  ggplot(aes(x = clarity, y = price)) + 
  geom_boxplot() + 
  ggtitle("Ideal, trimmed") +
  theme_minimal()

ds_trimmed %>% filter(cut == "Good") %>% 
  ggplot(aes(x = clarity, y = price)) + 
  geom_boxplot() + 
  ggtitle("Good, trimmed") +
  theme_minimal()

