# Clear Enviromment
if(!is.null(dev.list())) dev.off()
cat("\014")
rm(list = ls())
options(scipen = 9)

# setting the current directory
setwd('C:/Users/RXPCOMPUTER/Source/RProjects/dataanalyticswithr')

# Data transformation using the dplyr