##########################################
# Author : ltkhanh@bigdolphin.com.vn
# Date   : Dec 31, 2022
# License: GNU v2.0
##########################################
# Include libraries
library(rstudioapi)
library(imager)
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
# Set the plotting area into a 2*2 array
par(mfrow=c(2,2))

# Runtime
rt <- 1000
# Start timer for measuring runtime
tic()

# Main application
Ne = 800
Ni = 200
re <- runif(Ne)
ri <- runif(Ni)
tb_r_Excitatory <- matrix(runif((Ne+Ni)*Ne),nrow=Ne+Ni)
tb_r_Inhibitory <- matrix(runif((Ne+Ni)*Ni),nrow=Ne+Ni)

a <- c(0.02*rep(1,Ne),      0.02+0.08*ri)
b <- c(0.2*rep(1,Ne),       0.25-0.05*ri)
c <- c(-65+15*re^2,         -65*rep(1,Ni))
d <- c(8-6*re^2,            2*rep(1,Ni))
S <- cbind(0.5*tb_r_Excitatory,    -tb_r_Inhibitory)

# Initial values of v
v <- -65*rep(1,Ne+Ni)
vv <- rep(0,0)
# Initial values of u
u <- b * v
# Spike timings
firings <- matrix(c(0,0),ncol=2)
# Simulation of 1000 ms
for(t in 1:rt) {
  I <- c(5*rnorm(Ne),2*rnorm(Ni))
  fired <- which(v>=30)
  firings <- rbind(firings,cbind(t+0*fired,fired))
  v[fired] <- c[fired]
  u[fired] <- u[fired] + d[fired]
  y <- matrix(S[,fired],nrow=Ne+Ni)
  g <- rowSums(y)
  I <- I + g
  # Step 0.5 ms for numerical stability
  v <- v+0.5*(0.04*v^2+5*v+140-u+I)
  v <- v+0.5*(0.04*v^2+5*v+140-u+I)
  u <- u+a*(b*v-u)
  vv[t] <- v[1]
}
# Stop timer for measuring runtime
toc()
# Plot data
plot(firings, main="Firings", pch = 20, cex = 0.1)
plot(I, main=paste("I @ t=",1000,"ms"), pch = 1)
plot(vv, main="Membrane potential of the first cell",
     type="l",ylim = c(-80, 30))
plot(S, main="S", pch = 1, cex = 0.5)
