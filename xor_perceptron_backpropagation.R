##########################################
# Author : ltkhanh@bigdolphin.com.vn
# Date   : Dec 31, 2022
# License: GNU v2.0
##########################################
# Include libraries
library(rstudioapi)
library(pheatmap)
library(gridExtra)
library(tictoc)
library(igraph)

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

# Error Calculation
network_Error <- function(out,target){
  return(((target-out)^2)/2)
}
# Forwarding
cell2_Forward <- function(input,iw){
  s <- input[["x"]] * iw[1] + input[["y"]] * iw[2]
  return (1 / (1 + exp(-s)))
}
# BackPropagation
cell2_Backward <- function(samples,iw,irate){
  for(i in 1:2000){
    oz <- c(cell2_Forward(samples[["s0"]],iw),
            cell2_Forward(samples[["s1"]],iw),
            cell2_Forward(samples[["s2"]],iw),
            cell2_Forward(samples[["s3"]],iw))
    dataXY <- matrix(c(c(samples[["s0"]][["x"]],samples[["s0"]][["y"]]),
                       c(samples[["s1"]][["x"]],samples[["s1"]][["y"]]),
                       c(samples[["s2"]][["x"]],samples[["s2"]][["y"]]),
                       c(samples[["s3"]][["x"]],samples[["s3"]][["y"]])),
                     nrow = 2)
    tg <- c(samples[["s0"]][["z"]],
            samples[["s1"]][["z"]],
            samples[["s2"]][["z"]],
            samples[["s3"]][["z"]])
    dEds <- matrix((oz-tg)*oz*(1-oz),byrow = TRUE)
    dEdw <- dataXY %*% dEds
    wOld <- matrix(c(iw[1],iw[2]))
    wNew <- wOld - irate * dEdw
    iw <- c(wNew)
  }
  return(iw)
}
# Network
network_Forward <- function(input,iw){
  wires <-rep(0,3)
  wires[1] <- cell2_Forward(input,iw[[1]])
  wires[2] <- cell2_Forward(input,iw[[2]])
  wires[3] <- cell2_Forward(list("x"=wires[1],"y"=wires[2]),iw[[3]])
  #print(wires)
  return (wires)
}
# Train
network_Train <- function(samples,weight,rate,ntrain){
  dataXY <- matrix(c(c(samples[["s0"]][["x"]],samples[["s0"]][["y"]]),
                     c(samples[["s1"]][["x"]],samples[["s1"]][["y"]]),
                     c(samples[["s2"]][["x"]],samples[["s2"]][["y"]]),
                     c(samples[["s3"]][["x"]],samples[["s3"]][["y"]])),
                   nrow = 2)
  tg <- c(samples[["s0"]][["z"]],
          samples[["s1"]][["z"]],
          samples[["s2"]][["z"]],
          samples[["s3"]][["z"]])
  for(i in 1:ntrain){
    # Forwarding
    w0 <- network_Forward(samples[["s0"]],
                          list(c(weight[1],weight[2]),
                               c(weight[3],weight[4]),
                               c(weight[5],weight[6])))
    w1 <- network_Forward(samples[["s1"]],
                          list(c(weight[1],weight[2]),
                               c(weight[3],weight[4]),
                               c(weight[5],weight[6])))
    w2 <- network_Forward(samples[["s2"]],
                          list(c(weight[1],weight[2]),
                               c(weight[3],weight[4]),
                               c(weight[5],weight[6])))
    w3 <- network_Forward(samples[["s3"]],
                          list(c(weight[1],weight[2]),
                               c(weight[3],weight[4]),
                               c(weight[5],weight[6])))
    # Collecting data
    oz <- c(w0[3],w1[3],w2[3],w3[3])
    dataW1 <- c(w0[1],w1[1],w2[1],w3[1])
    dataW2 <- c(w0[2],w1[2],w2[2],w3[2])
    dataW1W2 <- matrix(c(dataW1,dataW2), byrow = TRUE, nrow = 2)
    
    dEds3    <- matrix((oz-tg)*oz*(1-oz),byrow = TRUE)
    dEdw12   <- dataXY %*% (dEds3 * (1-dataW1) * dataW1 * weight[5])
    dEdw34   <- dataXY %*% (dEds3 * (1-dataW2) * dataW2 * weight[6])
    dEdw56   <- dataW1W2 %*% dEds3
    
    wOld <- matrix(c(weight[1],weight[2]))
    wNew <- c(wOld - rate * dEdw12)
    weight[1] <- wNew[1]
    weight[2] <- wNew[2]
    wOld <- matrix(c(weight[3],weight[4]))
    wNew <- c(wOld - rate * dEdw34)
    weight[3] <- wNew[1]
    weight[4] <- wNew[2]
    wOld <- matrix(c(weight[5],weight[6]))
    wNew <- c(wOld - rate * dEdw56)
    weight[5] <- wNew[1]
    weight[6] <- wNew[2]
    
    if(ntrain<10){
      print(dEds3)
      print(dEdw12)
      print(dEdw34)
      print(dEdw56)
    }
  }
  return(weight)
}

minVal <- 0
maxVal <- 1
samples   <- list("s0"=list("x"=0,"y"=0,"z"=0),
                  "s1"=list("x"=0,"y"=1,"z"=1),
                  "s2"=list("x"=1,"y"=0,"z"=1),
                  "s3"=list("x"=1,"y"=1,"z"=0))
netWeight <- runif(2*3, min=minVal, max=maxVal)

# Start timer for measuring runtime
print("Initialized Network Weight:")
print(netWeight[1:4])
print(netWeight[5:6])
print("Network forwarding...")
tic()
n <- c(network_Forward(samples[["s0"]],
                     list(c(netWeight[1],netWeight[2]),
                          c(netWeight[3],netWeight[4]),
                          c(netWeight[5],netWeight[6])))[3],
       network_Forward(samples[["s1"]],
                       list(c(netWeight[1],netWeight[2]),
                            c(netWeight[3],netWeight[4]),
                            c(netWeight[5],netWeight[6])))[3],
       network_Forward(samples[["s2"]],
                       list(c(netWeight[1],netWeight[2]),
                            c(netWeight[3],netWeight[4]),
                            c(netWeight[5],netWeight[6])))[3],
       network_Forward(samples[["s3"]],
                       list(c(netWeight[1],netWeight[2]),
                            c(netWeight[3],netWeight[4]),
                            c(netWeight[5],netWeight[6])))[3])
toc()
print(n)
print("Network Training...")
tic()
w <- network_Train(samples,netWeight,0.2,5000)
toc()
print(w[1:4])
print(w[5:6])
print("Network re-forwarding...")
tic()
n <- c(network_Forward(samples[["s0"]],
                       list(c(w[1],w[2]),
                            c(w[3],w[4]),
                            c(w[5],w[6])))[3],
       network_Forward(samples[["s1"]],
                       list(c(w[1],w[2]),
                            c(w[3],w[4]),
                            c(w[5],w[6])))[3],
       network_Forward(samples[["s2"]],
                       list(c(w[1],w[2]),
                            c(w[3],w[4]),
                            c(w[5],w[6])))[3],
       network_Forward(samples[["s3"]],
                       list(c(w[1],w[2]),
                            c(w[3],w[4]),
                            c(w[5],w[6])))[3])
toc()
print(n)
