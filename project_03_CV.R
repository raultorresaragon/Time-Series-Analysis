# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Author: Raul
# Date: 2022-05-30
# Notes:
#   This checks 
#   - FE ARIMA(2,0,0)
#   - RE ARIMA(2,0,0)
#   out of sample performance using sliding window CV
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)
library(plm)
library(tseries)
library(nlme)
library(simcf)
library(lmtest)


rm(list = ls())
source("project_utils.R")
wd <- read_csv("an_data.csv") 
pdata <- pdata.frame(wd)

# creating first 2 lags of outcome
x <- pdata |> arrange(state, year) |> 
  dplyr::select(total_firearm_deaths)  |> 
  `names<-`(NULL) |> unlist()
c <- pdata |> arrange(state, year) |> 
  dplyr::select(state) |> 
  `names<-`(NULL) |> unlist()
t <- pdata |> arrange(state, year) |> 
  dplyr::select(year) |> 
  `names<-`(NULL) |> unlist()

pdata <- pdata |> arrange(state, year) |> 
  mutate(tfd_l1 = lagpanel(x=x, c=c, t=t, lagnum=1) |> as.vector(),
         tfd_l2 = lagpanel(x=x, c=c, t=t, lagnum=2) |> as.vector())

rm(list = c("x","c","t"))

