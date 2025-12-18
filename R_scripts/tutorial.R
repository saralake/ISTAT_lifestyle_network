# tutorial http://psychonetrics.org/files/SEMtutorial/tutorial_psychonetrics.html
# http://psychonetrics.org/files/PNAWS2020lecture.html

rm(list=ls())

library(psychonetrics)
library(lavaan)
library(dplyr)

data <- PoliticalDemocracy %>% 
  select(y1:y4)

Lambda_dem60 <- matrix(c(
  1, # dem60 =~ y1
  1, # dem60 =~ y2
  1, # dem60 =~ y3
  1 # dem60 =~ y4
),ncol=1,byrow=TRUE)

cfa_dem <- lvm(
  data, 
  lambda = Lambda_dem60, 
  latents = c("dem60")
)

cfa_dem <- cfa_dem %>% 
  runmodel

cfa_dem %>% fit

cfa_dem %>% parameters

cfa_dem %>% MIs
