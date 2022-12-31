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
# Set the plotting area into a 1*2 array
par(mfrow=c(1,2))

# Runtime
tau = 0.25
rt <- (600 / tau)
tspan <- c(0:rt)/4
# Start timer for measuring runtime
tic()

# Main application
a <- 0.1
b <- 0.26
c <- -60
d <- -1

# Initial values of v
v <- -62
# Initial values of u
u <- b*v

# Basic Impulses
#T1 = 10 ; T2 = 70;
#T3 = 150; T4 = 190;
#T5 = 300; T6 = 320;
#T7 = 400; T8 = 410;
#T9 = 500; T10 = 520; T11 = 540;
# Advanced impulses
T1 = 10 ; T2 = 30; T3 = 50;
T4 = 150; T5 = 160;T6 = 170; T7 = 180;
T8 = 260;
T9 = 300; T10 = 320; T11 = 340;

vv <- rep(0,0)
uu <- rep(0,0)
ii <- rep(0,0)
index = 0;
# Simulation of rt ms
sapply(tspan,function(t){
  I <- 0;
  if( ((t>T1)&&(t<T1+4))
      || ((t>T2) && (t < T2+4))
      || ((t>T3) && (t < T3+4))
      || ((t>T4) && (t < T4+4))
      || ((t>T5) && (t < T5+4))
      || ((t>T6) && (t < T6+4))
      || ((t>T7) && (t < T7+4))
      || ((t>T8) && (t < T8+4))
      || ((t>T9) && (t < T9+4))
      || ((t>T10) && (t < T10+4))
      || ((t>T11) && (t < T11+4))
    )
  {
    I <- 0.65;
  }
  v <<- v + tau*(0.04*v^2+5*v+140-u+I)
  u <<- u + tau*a*(b*v-u)
  if (v>30){
    v <<- c
    u <<- u+d
  }
  vv[index] <<- v 
  uu[index] <<- u
  ii[index] <<- I
  index <<- index + 1
})
# Stop timer for measuring runtime
toc()
# Plot data
plot(vv, main="Membrane potential of Resonator", 
        type="l", ylim = c(-80, 30),
        xaxt="n",xlab = "millisecond",
        ylab = "millivolt")
lines(10*ii-80, col="red")
axis(1,at=seq(1,index,by = 200),labels = tspan[(seq(1,index,by = 200))])
plot(uu, main="Membrane recovery",
        type="l",
        xaxt="n",xlab = "millisecond",
        ylab = "millivolt")
axis(1,at=seq(1,index,by = 200),labels = tspan[(seq(1,index,by = 200))])
