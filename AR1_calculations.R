rm(list = ls())
library(tseries)
library('R.matlab')
library(raster)
library('forecast')
library(ncdf4)
library(rasterVis)
library(lattice)
library(xlsx)
library(rJava)
library(xlsxjars)
library(nortest)
library(stats)
library(som)
library(Kendall)
library(KernSmooth)
library(moments)
library(zoo)


timets<-ts(as.double(data1),start=c(2000,1),frequency=12)
results<-stl(timets, s.window="periodic", s.degree=0, t.window=13, t.degree=1, robust=TRUE, na.action=na.fail)
ts1<-results$time.series
ts2<-as.double(ts1[,3])#De-seasonalized and residualized kNDVI


#AR
ar1 <- function(ts, wl) {
  # ts - time series to calculate indicator on
  # wl - window length used in calculation
  # returns a time series of AR(1) values
  l <- length(ts)
  ar1_ts <- rep(NA, l)
  hwl=wl/2 
  for (i in (hwl+1):(l-hwl)) {
    ar1_ts[i] <- ar.ols(ts[(i-hwl):(i+hwl-1)], aic=FALSE, order.max=1)$ar
  }
  return(ar1_ts)
}
#VAR
vari <- function(ts, wl) {
  # ts - time series to calculate indicator on
  # wl - window length used in calculation
  # returns a time series of variance values
  l <- length(ts)
  vari_ts <- rep(NA, l)
  hwl=wl/2 
  for (i in (hwl+1):(l-hwl)) {
    vari_ts[i] <- var(ts[(i-hwl):(i+hwl-1)])
  }
  return(vari_ts)
}














