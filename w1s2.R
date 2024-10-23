# Clear Enviromment
if(!is.null(dev.list())) dev.off()
cat("\014")
rm(list = ls())
options(scipen = 9)

# setting the current directory
setwd('C:/Users/RXPCOMPUTER/Source/RProjects/dataanalyticswithr')


df <- read.csv('iris.csv')
# exploratory data analysis (EDA)

PL <- df$petal_length
PW <- df$petal_width
plot(PL, PW)

plot(PL, PW, pch=2) # pch = 2 means the symbols is triangle
# The pch parameter can take values from 0 to 25.

plot(PL, PW, pch=2, col="green") # col ="green" change the symbol color to green

species <- as.factor(df$species)
# the data type of the Species column is character. We need to convert this column into a factor.
str(species)

# factors are used to store categorical variables as levels.
# To completely convert this factor to numbers for plotting, we use the as.numeric function.
speciesId <- as.numeric(species)
speciesId

plot(PL, PW, pch=speciesId, col="green") 

# assign 3 colors red, green, and blue to 3 species *setosa*, *versicolor*,
# and *virginica* respectively
plot(PL, PW, pch=speciesId, col=speciesId) 


# Let us change the x- and y-labels, and add a main title.

plot(PL, PW, # x and y
     pch=speciesId, # symbol type
     col=speciesId, # color
     xlab = "Petal length (cm)", # x label
     ylab = "Petal with (cm)", # y label
     main = "Petal width vs. length"
     ) 
# We notice a strong linear correlation between petal length and width.
# Let’s add a trend line using abline(), a low level graphics function.
abline(lm(PW ~ PL)) # the order is reversed as we need y ~ x.

PCC <- cor(PW, PL) # Pearson's correlation coefficient
PCC <- round(PCC, 2) # round to the 2nd place after decimal point.
paste("R =", PCC)

# Then we use the text function to place strings at lower right 
# by specifying the coordinate of (x=5, y=0.5).
text(5, 0.5, paste("R =", PCC)) # add text annotation.
legend("topleft", # specify the location of the legend
       levels(species),# specify the levels of species
       pch = 1:3, # specify three symbols used for the three species
       col = 1:3 # specify three colors for the three species
       )

# While data frames can have a mixture of numbers and characters in different columns,
# a matrix often only contains numbers.
ma <- as.matrix(df[, 1:4]) # convert to matrix
colMeans(ma) # column means for matrix

# The same thing can be done with rows via rowMeans(x) and rowSums(x).
# We can generate a matrix of scatter plot by pairs() function.
pairs(ma)

pairs(ma, col = rainbow(3)[speciesId])

df1 <- df[, 1:4]
stars(df1) # do I see any diamonds?
stars(df1, key.loc = c(17, 0)) # What does this tell you?


stars(df1, key.loc = c(20, 0.5), draw.segments = TRUE) # What does this tell you?
