##########################################
# Author : ltkhanh@bigdolphin.com.vn
# Date   : Oct 14, 2022
# License: GNU v2.0
##########################################

# Include libraries
library(rstudioapi)
library(pheatmap)
library(gridExtra)
library(tictoc)

# Clean all global environments
rm(list = ls())

# Function: clean R console
# Input: none
# Return: none
cls <- function() {
  cat("\014")
}

# Remove All Plots from RStudio
if (is.null(dev.list()['RStudioGD'])){
  dev.new()
} else{
  dev.off()
}
dev.new()
cls()

# set seed for reproducible
#set.seed(2022)
# Set the plotting area into a 1*1 array
par(mfrow=c(1,1))

# Start timer for measuring runtime
tic()

# Main application
# vector of values
minVal <- 1
maxVal <- 6
# random value of rolling ns times
ns <- 2000
pro <- rep(0,12)
for(i in 1:ns){
  valDice1 <- round(runif(1, min=minVal, max=maxVal), 0)
  valDice2 <- round(runif(1, min=minVal, max=maxVal), 0)
  sumVal <- valDice1 + valDice2
  pro[sumVal] <- pro[sumVal] + 1
}
pro <- (pro / ns) * 100
maxPro <- max(pro)
mostVal <- which(pro>=maxPro)
# Stop timer for measuring runtime
toc()
print("Propabilities:")
print(pro[2:12])
barplot(pro[2:12], names.arg=seq(from=2,to=12,by=1), col="#1b98e0")
print("Mostly occur values")
print(mostVal)
