rm(list=ls())

# select only variables of interest from Docs/var_list.xlsx
# concatenate PROFAM and PROIND to get participant ID
# remove underage participants
# RESTRUCTURE SOME VARS:
# substitute 99 with NAs in numerical variables (99 corresponds to 'don't know') 
# compute years of marriage (2023 - AMATR)
# create aggregated vars based on Docs/var_list.xlsx

import_folder = 'Original_data/'
export_folder = 'Intermediate_data/ver1/'

if(!dir.exists(export_folder)){
  dir.create(export_folder)
}

library(readxl)
library(dplyr)

all_dat = read.table(paste0(import_folder, 'AVQ_Microdati_2023.txt'), header = TRUE, sep = "\t")

# select variables of interest 

my_vars_dat = read_excel('Docs/var_list.xlsx')
my_vars_dat = my_vars_dat[,c(1:6)]
my_vars = my_vars_dat$label

dat <- all_dat[, my_vars, drop = FALSE] # keep only columns listed in my_vars

# concatenate PROFAM and PROIND to get participant ID
dat$ID = paste0(dat$PROFAM, "_", dat$PROIND)
# drop them from vars of interest
vars_interest = my_vars[3:length(my_vars)]
# drop them from database
dat = dat[, !names(dat) %in% c('PROFAM', 'PROIND')]

# drop rows for underage participants (1 to 6) 007 da 18 a 19 anni   008 da 20 a 24 anni   009 da 25 a 34 anni   010 da 35 a 44 anni   011 da 45 a 54 anni   012 da 55 a 59 anni    # 013 da 60 a 64 anni   014 da 65 a 74 anni   015 75 anni e piu' 
dat = dat[dat$ETAMi>=7, ]

# RESTRUCTURE SOME VARS:
# 99 corresponds to 'don't know'. count how many there are and decide whether to remove these lines or substitute them with NAs so that we can impute them later
# count how many NAs per column and decide imputation strategy. Nelle variabili tipo "NPROSOM" e simili, imputare con 0 perchè significa "mai"
# Nelle variabili tipo "NLIBRIM" imputare con 0 che significa 'nessun libro' ecc

res99 = colSums(dat==99, na.rm = T)
res99[res99>0]

# ISTRMi: titolo di studio: 01	laurea e post-laurea; 07	diploma; 09	licenza di scuola media; 10	licenza di scuola elementare, nessun titolo di studio; 99	non disponibile.
# POSIZMi: posizione lavorativa. 01	dirigente; autonomo come imprenditore; libero professionista; 02	direttivo, quadro; impiegato; 03	capo operaio, operaio subalterno e assimilati; apprendista; lavorante a domicilio per conto d'impresa; 04	lavoratore in proprio; socio cooperativa Produzione Beni e/o prestazioni di servizio; coadiuvante; collaborazione coordinata e continuativa (con o senza progetto); prestazione d'opera occasionale; 99	non disponibile
# HHRAD besides having many 99s, also has 40% NAs. remove.   HHTEL contain many 99 vals. Remove these vars

dat$HHRAD = NULL
dat$HHTEL = NULL

colSums(is.na(dat))
percentage_NAs = colSums(is.na(dat)) / nrow(dat) * 100
percentage_NAs[percentage_NAs>30]

NA_vars = names(percentage_NAs[percentage_NAs>30]) 
impute0 = NA_vars[c(5:length(NA_vars)-1)]

for (i in impute0){
  dat[[i]][is.na(dat[[i]])] = 0
}

percentage_NAs = colSums(is.na(dat)) / nrow(dat) * 100
percentage_NAs[percentage_NAs>30]

# now only AMATR LAVPAS TLAV SODLAV2 have missing values. but they are because not everyone in the sample is married and not everyone in the sample is working.

names(percentage_NAs[percentage_NAs>0 & percentage_NAs<30]) # these are the values to impute with iterative imputer before going on with preprocessing

train_dat <- dat[seq(1, nrow(dat), by = 2), ]  # odd rows
test_dat  <- dat[seq(2, nrow(dat), by = 2), ]  # even rows

save(dat, file=paste0(export_folder, 'dat_99s_NAs_corrected.RData'))
save(train_dat, file=paste0(export_folder, 'train_dat_before_imputing.RData'))
save(test_dat, file=paste0(export_folder, 'test_dat_before_imputing.RData'))

write.csv(dat, file=paste0(export_folder, 'dat_99s_NAs_corrected.csv'), dec = ".", col.names = TRUE, row.names = F)
write.csv(train_dat, file = paste0(export_folder, 'train_dat_before_imputing.csv'), dec = ".", col.names = TRUE, row.names = F)
write.csv(test_dat, file = paste0(export_folder, 'test_dat_before_imputing.csv'), dec = ".", col.names = TRUE, row.names = F)

# compute years of marriage (2023 - AMATR)
# create aggregated vars based on Docs/var_list.xlsx

# get a table with ranges and quantiles
tab_all = NULL

for (i in vars_interest[vars_interest != 'AMATR']){
  
  tab <- data.frame(matrix(ncol = 5, nrow = 1))
  colnames(tab) <- names(quantile(dat[[i]], na.rm = T))
  tab[1,] = quantile(dat[[i]], na.rm = T)
  tab$var = i
  
  tab_all = rbind(tab_all, tab)
}

write.table(tab_all, file=paste0(export_folder, 'vars_ranges.csv'), sep = ' ', na = "NA", dec = ",", col.names = T, row.names = F)

# substitute 99 with NAs
# which variables have 99? 
vars_to_edit = tab_all[tab_all$`100%`==99, ]$var

for (i in vars_to_edit){
  
  dat[[i]][dat[[i]]==99] = NA
  
}

# compute ranges again
tab_all = NULL

for (i in vars_interest[vars_interest != 'AMATR']){
  
  tab <- data.frame(matrix(ncol = 5, nrow = 1))
  colnames(tab) <- names(quantile(dat[[i]], na.rm = T))
  tab[1,] = quantile(dat[[i]], na.rm = T)
  tab$var = i
  
  tab_all = rbind(tab_all, tab)
}

write.table(tab_all, file=paste0(export_folder, 'vars_ranges_clean.csv'), sep = ' ', na = "NA", dec = ",", col.names = T, row.names = F)

# compute years of marriage (2023 - AMATR): MARR_YEARS (how long the person has been married, both widowed and with spouse alive)
dat$MARR_YEARS = 2023-dat$AMATR


# create aggregated vars based on Docs/var_list.xlsx
# aggregate: find vars to aggregate
vars_to_aggr = my_vars_dat %>%
  filter(note == "aggregare") %>%
  select(label, sost) %>%
  as.data.frame()

vars_pre_aggr = colnames(dat)

for (new_var in unique(vars_to_aggr$sost)){
  cols_to_aggr = vars_to_aggr[vars_to_aggr$sost==new_var, ]$label
  
  dat$aggregated <- rowMeans(dat[, cols_to_aggr], na.rm = F)
  
  colnames(dat)[colnames(dat)=='aggregated'] = new_var
  
  dat = dat[, !names(dat) %in% cols_to_aggr]
  
}

# check drop of redundant columns
setdiff(vars_pre_aggr, colnames(dat))
setdiff(colnames(dat), vars_pre_aggr)
setdiff(unique(vars_to_aggr$sost), colnames(dat))

# get vars distributions
fig_folder = 'Figures/data_distr/'

final_vars = colnames(dat)[!colnames(dat) %in% c('ID','SESSO', 'AMATR')]

if(!dir.exists(fig_folder)){
  dir.create(fig_folder)
}

for (i in final_vars){
  
  png(paste0(fig_folder, 'hist_', i, '.png'))
  hist(dat[[i]], xlab=i, main=paste0('Histogram of ', i))
  dev.off()
  
}

# save dat
save(dat, file=paste0(export_folder, 'clean_dataset.RData'))

