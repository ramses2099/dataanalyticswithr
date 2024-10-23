# Clear Enviromment
if(!is.null(dev.list())) dev.off()
cat("\014")
rm(list = ls())
options(scipen = 9)

# setting the current directory
setwd('C:/Users/RXPCOMPUTER/Source/RProjects/dataanalyticswithr')

# Data transformation using the dplyr

# install packages
install.packages("dplyr")
install.packages("tidyverse")

# load packages
library(dplyr)
library(tidyverse)
library(readr)

df <- read.table("heartatk4R.txt", "\t", header = TRUE)


head(df)

str(df)

# We can reformat categorical values factors. 
# We are going to use df$SEX to refer to the SEX column of the data frame df:

df$DIAGNOSIS <- as.factor(df$DIAGNOSIS)
df$SEX <- as.factor(df$SEX)
df$DRG <- as.factor(df$DRG)
df$DIED <- as.factor(df$DIED)

nlevels(df$DIAGNOSIS)
levels(df$DIAGNOSIS)

summary(df) # summary often gives us a lot of useful information

df2 <- df[order(df$AGE), ]

df2$pdc <- df2$CHARGES / df2$LOS

# pdc per day cost

df2$ag <- floor(df2$AGE/10) * 10

head(df2)

CHARGES <- df2$CHARGES
AG <- df2$ag

boxplot(CHARGES ~ AG)



# You can extract a subset of cases:

df3 <- subset(df2, SEX == "F")
df4 <- subset(df3, AGE > 80)




# filter data using function in dplyr
df2 <- df %>% 
  arrange(AGE)
head(df2)



# add new column pdc: per day cost
df2 <- df2 %>% 
  mutate(pdc = CHARGES / LOS)
head(df2)

# add new column ag: age group
df2 <- df2 %>% 
  mutate(ag = floor(AGE/10) * 10)
head(df2)

# filter data wiht filter function
df3 <- df %>% 
  filter(SEX =="F", AGE > 80)
head(df3)


