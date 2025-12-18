rm(list=ls())

# RESTRUCTURE SOME VARS:
# compute years of marriage (2023 - AMATR)
# create aggregated vars based on Docs/var_list.xlsx

import_folder = 'Intermediate_data/ver1/'
export_folder = 'Results/data_inspection/ver1/'

if(!dir.exists(export_folder)){
  dir.create(export_folder)
}

library(readxl)
library(dplyr)

# compute years of marriage (2023 - AMATR)
# create aggregated vars based on Docs/var_list.xlsx

files = dir(import_folder)[grepl('imputed.csv', dir(import_folder))]

dat_rec = NULL

for(iF in files){
  dat=read.csv(paste0(import_folder, iF))
  dat_rec = rbind(dat_rec, dat)
  rm(dat)
}

my_vars_dat = read_excel('Docs/var_list.xlsx')
my_vars_dat = my_vars_dat[,c(1:6)]
my_vars = my_vars_dat$label
vars_interest = my_vars[3:length(my_vars)]
extra_vars = setdiff(vars_interest, names(dat_rec))
vars_interest=vars_interest[! vars_interest %in% extra_vars]

# compute years of marriage (2023 - AMATR): MARR_YEARS (how long the person has been married, both widowed and with spouse alive)
dat_rec$MARR_YEARS = 2023-dat_rec$AMATR

# get a table with ranges and quantiles
tab_all = NULL

for (i in names(dat_rec)[! names(dat_rec) %in% 'ID']){
  
  tab <- data.frame(matrix(ncol = 5, nrow = 1))
  colnames(tab) <- names(quantile(dat_rec[[i]], na.rm = T))
  tab[1,] = quantile(dat_rec[[i]], na.rm = T)
  tab$var = i
  
  tab_all = rbind(tab_all, tab)
}

write.table(tab_all, file=paste0(export_folder, 'vars_ranges.csv'), sep = ' ', na = "NA", dec = ",", col.names = T, row.names = F)

# create aggregated vars based on Docs/var_list.xlsx
# aggregate: find vars to aggregate
vars_to_aggr = my_vars_dat %>%
  filter(note == "aggregare") %>%
  select(label, sost) %>%
  as.data.frame()

for (new_var in unique(vars_to_aggr$sost)){
  cols_to_aggr = vars_to_aggr[vars_to_aggr$sost==new_var, ]$label
  
  dat_rec$aggregated <- rowMeans(dat_rec[, cols_to_aggr], na.rm = F)
  
  colnames(dat_rec)[colnames(dat_rec)=='aggregated'] = new_var
  
  dat_rec = dat_rec[, !names(dat_rec) %in% cols_to_aggr]
  
}

# check drop of redundant columns
vars_pre_aggr = names(dat_rec)[! names(dat_rec) %in% 'ID']
setdiff(vars_pre_aggr, colnames(dat_rec))
setdiff(colnames(dat_rec), vars_pre_aggr)
setdiff(unique(vars_to_aggr$sost), colnames(dat_rec))

# get new table with ranges and quantiles for aggregated data
tab_all_aggr = NULL

for (i in names(dat_rec)[! names(dat_rec) %in% c('ID', 'AMATR', 'SESSO', 'STCIVMi', 'LAVPAS', 'CITTMi')]){
  
  tab <- data.frame(matrix(ncol = 5, nrow = 1))
  colnames(tab) <- names(quantile(dat_rec[[i]], na.rm = T))
  tab[1,] = quantile(dat_rec[[i]], na.rm = T)
  tab$var = i
  
  tab_all_aggr = rbind(tab_all_aggr, tab)
}

write.table(tab_all_aggr, file=paste0(export_folder, 'aggregated_vars_ranges.csv'), sep = ' ', na = "NA", dec = ",", col.names = T, row.names = F)

# get vars distributions
final_vars = colnames(dat_rec)[!colnames(dat_rec) %in% c('ID','SESSO', 'AMATR')]

for (i in final_vars){
  
  png(paste0(export_folder, 'hist_', i, '.png'))
  hist(dat_rec[[i]], xlab=i, main=paste0('Histogram of ', i))
  dev.off()
  
}

# aggregate and save separately train and test data!
for(iF in files){
  
  filename = gsub('_dat_imputed.csv', '', iF)
  dat=read.csv(paste0(import_folder, iF))
  dat$MARR_YEARS = 2023-dat$AMATR
  
  for (new_var in unique(vars_to_aggr$sost)){
    cols_to_aggr = vars_to_aggr[vars_to_aggr$sost==new_var, ]$label
    
    dat$aggregated <- rowMeans(dat[, cols_to_aggr], na.rm = F)
    
    colnames(dat)[colnames(dat)=='aggregated'] = new_var
    
    dat = dat[, !names(dat) %in% cols_to_aggr]
    
  }
  
  save(dat, file=paste0(import_folder, filename, '_dat_imputed_aggregated.RData'))
  write.table(dat, file=paste0(import_folder, filename, '_dat_imputed_aggregated.csv'), sep = ' ', na = "NA", dec = ",", col.names = T, row.names = F)
  rm(dat)
  
}



