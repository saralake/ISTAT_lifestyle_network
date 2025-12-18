rm(list=ls())

library(corrplot)
library(psych)
library(psychonetrics)
library(dplyr)

# FACTOR ANALYSIS ON TRAIN DATA to discover factor structure

import_folder = 'Results/training_set_factor_strucure/ver1/'
data_folder = 'Intermediate_data/'
export_folder = 'Results/network_analysis/'

if(!dir.exists(export_folder)){
  dir.create(export_folder)
}

source('R_functions/sort_by_loadings.R')

load(paste0(import_folder, 'factor_loadings.RData'))
load(paste0(data_folder, 'train_dat_clean.RData'))

# eliminare variabili che caricano poco sui fattori (<abs(0.3))
loadings_clean = sort_by_loadings(loadings, drop.cut=0.3, drop=T)

sort_by_loadings(loadings)

#loadings_red = loadings_clean[loadings_clean$dominant_factor %in% c("ML1", "ML10", "ML5"), ]
#loadings_red = loadings_red[ colnames(loadings_red) %in% c("ML1", "ML10", "ML5", 'dominant_factor')]

# remove vars with small loadings from train_dat
#removed_vars = setdiff(colnames(train_dat), rownames(loadings_red))
#train_dat = train_dat[ ! colnames(train_dat) %in% removed_vars[!removed_vars %in% 'SESSO']]
#train_dat = train_dat[ ! colnames(train_dat) %in% removed_vars]

sum(is.na(train_dat))
#loadings_clean = loadings_red

# for each var, select only factor with greatest absolute loading and set it to 1; the other to 0 (obtain factor matrix for network anal)
loadmat=loadings_clean

for(i in rownames(loadmat)){
  domfac=loadmat[i,]$dominant_factor
  
  loadmat[i,][[domfac]]=1
  
  cols_to_0 = colnames(loadmat)[!colnames(loadmat) %in% c('dominant_factor', domfac)]

  loadmat[i, cols_to_0] = 0

}


# CONFIRMATORY FACTOR ANALYSIS

loadmat$dominant_factor=NULL
loadmat = as.matrix(loadmat)

cfamod <- lvm(train_dat, lambda = loadmat, 
                  identification = "variance",   
                  estimator = "FIML",       
                  latents = colnames(loadmat))
cfamod <- cfamod %>% runmodel

cfamod %>% fit # bad CFI > .90 ok; RMSEA < .08 not ok

cfamod@fitmeasures[c("chisq","pvalue","rmsea","cfi")]

cfamod %>% MIs

cfamod_WLS <- lvm(train_dat, lambda = loadmat, 
              identification = "variance",   
              estimator = "WLS",       
              latents = colnames(loadmat))
cfamod_WLS <- cfamod_WLS %>% runmodel

# 4. Inspect the model (Is it a good model?) CFI > .90 = acceptable fit; RMSEA < .08 = acceptable
cfamod_WLS@fitmeasures[c("chisq","pvalue","rmsea","cfi")] # this is ok: CFI > .90; RMSEA < .08 
cfamod_WLS %>% fit

compare(FIML_cfa=cfamod, DWLS_mod = cfamod_WLS)

# If the degrees of freedom are 0, then the model is just-identified (saturated) and will fit the data perfectly. Saturated models cannot be compared to empirical data
# we want DF to be positive (to have an overidentified model) because it means there is more than 1 way to estimate parameters

#rmsea.pvalue. This measures the amount of misfit your model has per degrees of freedom. The smaller this value, the better the fit. Always also look at the confidence intervals of this value. Here are some rough guidelines of thresholds you can use for interpretation:
# < .05 “very good fit” or “close fit”
# .05 - .08 “good fit” or “fair fit”
# .08 - .1 “mediocre fit” or “good”
# .10 “poor” or “unacceptable”

cfamod_WLS %>% parameters # nu Encodes the intercepts of the observed variables.
# lambda is A matrix encoding the factor loading structure: Var 1 = observed indicator; Var 2 = latent variable
# sigma_zeta is A matrix encoding the covariances between latent variables (check with the output from the factor analysis in the previous step)
# sigma_epsilon is A matrix encoding residual terms.

getmatrix(cfamod_WLS, "lambda")

cfamod_WLS %>% MIs 

#cfamod_WLS1 <- cfamod_WLS %>% 
#  freepar("sigma_epsilon","FUMO", 'BICFUORIM') %>% 
#  runmodel

#cfamod_WLS %>% parameters

#cfamod_WLS1 %>% fit

#cfamod_WLS1 %>% MIs # when do they show that something is wrong and we need to modify the model?

#compare(original = cfamod_WLS, freed = cfamod_WLS1) # worsening of the freed model, keep the original one

# LATENT NETWORK MODEL
lnmmod <- lnm(train_dat, 
              lambda = loadmat,
              identification = "variance", 
              estimator = "DWLS",              
              latents = colnames(loadmat))

# Run model by first pruning non significant edges and then automatically add edges at α=0.05 until BIC can no longer be improved.
lnmmod <- lnmmod %>% runmodel %>%  
  prune(adjust = "fdr", 
        alpha = 0.01) %>%   
  stepup(criterion = "bic", 
         alpha = 0.05)

lnmmod %>% parameters
lnmmod %>% fit
lnmmod@fitmeasures[c("chisq","pvalue","rmsea","cfi")] # ok
cfamod_WLS@fitmeasures[c("chisq","pvalue","rmsea","cfi")] # this is ok: CFI > .90; RMSEA < .08 

compare(CFA = cfamod, net = lnmmod) 

