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
ns <- 5000
x <- runif(ns)
y <- runif(ns)

nci <- 0
nc <- matrix(0,2,ns)
nnc <- matrix(0,2,ns)
for(i in 1:ns){
  r <- x[i] * x[i] + y[i] * y[i]
  if(r<=1){
    nci <- nci + 1
    nc[1,i] <- x[i]
    nc[2,i]<- y[i]
  } else {
    nnc[1,i] <- x[i]
    nnc[2,i]<- y[i]
  }
}
pi <- nci * 4 / ns
# Stop timer for measuring runtime
toc()
print(sprintf("Pi = %.10f",pi))
# Plot data
plot(nc[1,],nc[2,],col='red', pch = 20, cex = 0.5, xlab = "", ylab = "")
points(nnc[1,],nnc[2,],col='green', pch = 20, cex = 0.5)
