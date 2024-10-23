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

heartatk4R <- read.table("heartatk4R.txt", "\t", header = TRUE)


df <- heartatk4R # Make a copy of the data for manipulation, call it x. 
df$DIAGNOSIS <- as.factor(df$DIAGNOSIS)
df$SEX <- as.factor(df$SEX)
df$DRG <- as.factor(df$DRG)
df$DIED <- as.factor(df$DIED)
str(df)

# You can attach your data set if you want to refer to the columns directly by name, such as LOS instead of x$LOS
attach(df)

barplot(table(DIAGNOSIS))

counts <- sort(table(DIAGNOSIS), decreasing = TRUE) #table&Sort
percentages <- 100 * counts / length(DIAGNOSIS)
barplot(percentages, las =3, ylab = "Percentage", col = "green")


table(SEX)
pie(table(SEX))


# Possible correlation between two numeric columns
# PPC  Pearson’s correlation coefficients

# Note that Pearson’s correlation ranges from -1 to 1, with -1 and 1 indicating 
# perfect negative and possitive correlation respectively.
# Negative correlations are just as important and informative as positive ones.


# Table 5.2: Interpretation of correlation coefficient. 
# Correlation 	Negative 	Positive
# -     	-0.09 to 0.0 	0.0 to 0.09
# Small 	-0.3 to -0.1 	0.1 to 0.3
# Medium 	-0.5 to -0.3 	0.3 to 0.5
# Large 	-1.0 to -0.5 	0.5 to 1.0


cor.test(AGE, LOS)


# Associations between categorical variables
counts <- table(SEX, DIED)

# data in percentages
# counts <- counts / rowSums(counts)
# counts

chisq.test(counts)

# p-value < 2.2e-16

# Have you seen this p-value before? Probably! It is the smallest 
# non-zero number R shows for lots of tests.
# However, p is definitely small! Hence we reject the hypothesis that 
# the mortality rate is the same for men and women.

fisher.test(counts)


counts <- table(DIED, SEX)

# graph
text(50, 6500, labels = "A")
barplot(counts, legend = rownames(counts), col = rainbow(2),
        xlab ="DIED", beside = F, args.legend = list(x ="topleft"))
