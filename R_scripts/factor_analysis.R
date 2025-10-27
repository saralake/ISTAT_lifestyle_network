rm(list=ls())
# prova latent network

if(!require(pacman)) install.packages("pacman")
pacman::p_load("dplyr", # data management
               "qgraph",# network analysis
               "psych", # psychometrics and more by William Revelle
               "corpcor", # partial correlations
               "bootnet", # bootstrap on network
               "networktools", # bridge centrality
               "graphicalVAR", # N=1 time series VAR network
               "mlVAR", # multilevel time series VAR network
               "lm.beta", # regression analysis (used for detrending)
               "readr",
               "mgm",
               "NetworkComparisonTest",
               "EstimateGroupNetwork",
               "glasso",
               "haven",
               "networktools",
               "GPArotation",
               "lvnet",
               "psychTools",
               "moments",
               "corrplot",
               "pandas",
               "lavaan",
               "Rmpfr",
               "psychonetrics"
)
select <- dplyr::select
filter <- dplyr::filter

import_folder = 'Intermediate_data/'
export_folder = 'Intermediate_data/prova1/'

if(!dir.exists(export_folder)){
  dir.create(export_folder)
}

load(paste0(import_folder, 'clean_dataset.RData'))

# keep only vars
dat = dat[, !names(dat) %in% c("SESSO", "STCIVMi", "AMATR", 'ID')]

# THERE ARE MISSING VALUES! Iterative imputer in python separately for test and train, save and then run following steps

na_dat= na.omit(dat)

cortest.bartlett(dat)

# KMO TEST
KMO(dat)