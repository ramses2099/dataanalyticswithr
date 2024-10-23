# Clear Enviromment
if(!is.null(dev.list())) dev.off()
cat("\014")
rm(list = ls())
options(scipen = 9)

# setting the current directory
setwd('C:/Users/RXPCOMPUTER/Source/RProjects/dataanalyticswithr')


df <- read.csv('iris.csv')

head(df) # first few rows
class(df) # show the data type

str(df) # show the structure

summary(df) # summary statistics

df[3,4] # returns the value lies at the intersection of row 3 and column 4
df[3,] # returns the value of row 3
df[,4] # returns the value of column 4
df[3, 1:4] # returns values at the intersection of row 3 and columns from 1 to 4

colnames(df) # returns columns names


PL <- df$petal_length # define a new variable which contains only the petal length 
barplot(PL)
hist(PL)

mean(PL) #mean, duh
summary(PL)
# min 1
# max 6.9
# mean o average 3.75
# mid-point or median 4.35
# 3rd quartile, or 75th percentile 5.100
# 1st quartile, or 25th percentile 1.600

boxplot(PL) # boxplot of petal length
boxplot(df[,1:4]) # boxplot of four columns of iris

sd(PL) # standard deviation of petal length

SW <- df$sepal_width
sd(SW) # standard devition of sepal width

plot(PL) # scatter plot
# There are three different groups in the scatter plot.
# The petal length of one group is much smaller than the other two groups.

hist(PL) # histogram
# Histogram shows the distribution of data by plotting the frequency of data in bins.
# The histogram top right of Figure 1.6 shows that there are more flowers
# with Petal Length between 1 cm and 1.5 cm. It also shows that the data
# does not show a bell-curved distribution.


lag.plot(PL) # lag plot
# The lag plot is a scatter plot against the same set of number with an offset of 1.
# Any structure in a lag plot indicates non-randomness in the order in which 
# the data is presented. We can clearly see three clusters, indicating that values
# are centered around three levels sequentially.


qqnorm(PL) # Q Q plot for normal distribution
qqline(PL) # add the regression line
# Q-Q (quantile-quantile) plots can help check if data follows a Normal distribution, 
# which is widely observed in many situations. It is the pre-requisite 
# for many statistical methods. See Figure 1.7 for an example of normal distribution.
# Quantiles of the data is compared against those in a normal distribution.
# If the data points on a Q-Q plot form a straight line, the data has a normal distribution.


# We can do a one-sample t-test for the mean of a normally distributed data. 
# We want to test if its mean is different from 1.5 cm at the significance level α=0.05. 
t.test(PL[1:50], mu = 1.5) # two side t-test for the mean of petal length of setosa
# In this case, our null hypothesis is that the mean of petal length of setosa is 1.5 cm.
# Since p-value 0.1487 is greater than the significance level 0.05,
# there is no strong evidence to reject the null hypothesis.
# This function also tells us the 95% confidence 
# interval for the mean. Based on our sample of the 50 observations, we have 95% 
# confidence to conclude that the mean of petal length of setosa
# (if we were to measure all setosa in the world) is between 1.412645 cm and 1.511355 cm.


# We can perform a hypothesis test on whether a set of numbers are derived from normal distribution.
# When interpreting results from hypothesis testing, it is important to state the null hypothesis is.
# Here the null hypothesis is that the data is from a normal distribution.
shapiro.test(PL) # normality test

# If petal length is normally distributed, there is only 7.412×10-10 chance of getting
# the observed test statistic of 0.87627. In other words, it is highly unlikely that
# petal length follows a normal distribution. We reject the normal distribution hypothesis,
# which could be corroborated by our plots above.


# normality test of  the first 50 observations of PL
shapiro.test(PL[1:50])

# Given the significance level α=0.05, the p-value of normality test for 
# the pental length of setosa is 0.05481 which is greater than 0.05.
# Then we fail to reject the null hypothesis and conclude that we don’t have evidence
# to reject the hypthoses that the pental length of setosa follows a normal distribution.
# In statistics, we rely on both charts and statistical models to draw a conclusion.


# In the iris dataset, the last column contains the species information.
# We call this a categorical variable. Bar and Pie charts are very effective 
# in showing proportions.
# We can see that the three species are each represented with 50 observations.

count <- table(df$species)
count

pie(count)
barplot(count)

# Scatter plot is very effective in visualizing the correlation 
# between two columns of numbers.

PW <- df$petal_width
plot(PW, PL)

# shows that there is a positive correlation between petal length and petal width.
# In other words, flowers with longer petals are often wider. 
# So the petals are getting bigger substantially when both dimensions increase.


SP <- as.factor(df$species)
plot(PW, PL, col= SP)
text(1.5, 1.5, paste("R=0.96"))
legend("topleft", levels(SP), fill = 1:3)


# shows that there is a positive correlation between petal length and petal width.
# In other words, flowers with longer petals are often wider. 
# So the petals are getting bigger substantially when both dimensions increase.
cor(PW, PL)
# This means the petal width and petal length are strongly and positively correlated.

cor.test(PW, PL)
# Through hypothesis testing, we reject the null hypothesis that the
# true correlation is zero (no correlation).
# That means the correlation is statistically significant.

model <- lm(PW ~ PL) # linear model
summary(model)

abline(model)
plot(model)


# we want to know if the observed differences among the two groups reflect 
# real differences in the population or due to random sampling error.
boxplot(PL ~ SP)

PL1 <- PL[51:100] # extract petal length of versicolor
PL1

PL2 <- PL[101:150] # extract petal length of virginica
PL2

boxplot(PL1, PL2) # boxplot of the two groups of values
t.test(PL1, PL2) # two sample t-test



df2 <- df[51:150, ]
colnames(df2)
t.test(df2$petal_length ~  df2$species, data = df2)
boxplot(df2$petal_length ~  df2$species, data = droplevels(df2))

SP <- as.factor(df$species)
SW <- df$sepal_width
boxplot(SW ~ SP)


summary(aov(SW ~ SP))
# Since p-value is much smaller than 0.05, we reject the null hypothesis.
# We can conclude that not all the 3 species have the same mean of sepal width.
# In other words, there are at least two of the species have different means of sepal width.
