rm(list=ls())

# select only variables of interest from Docs/var_list.xlsx
# concatenate PROFAM and PROIND to get participant ID
# remove underage participants
# RESTRUCTURE SOME VARS:
# substitute 99 with NAs in numerical variables (99 corresponds to 'don't know') 
# compute years of marriage (2023 - AMATR)
# create aggregated vars based on Docs/var_list.xlsx

import_folder = 'Original_data/'
export_folder = 'Intermediate_data/'

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
# substitute 99 with NAs in numerical variables (99 corresponds to 'don't know') 
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

