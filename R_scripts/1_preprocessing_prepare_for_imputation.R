rm(list=ls())

# create a dataset as clean as possible (deleting non-interesting vars and those with too mani NAs/not available data; 
# restructuring vars depending on other questions); then aggregate those variables that refer to the same cluster and are on the same scale to avoid redundancy
# save clean and not so clean dataset

import_folder = 'Original_data/'
export_folder = 'Intermediate_data/ver2/'

if(!dir.exists(export_folder)){
  dir.create(export_folder)
}

library(readxl)
library(dplyr)
library(stringr)

all_dat = read.table(paste0(import_folder, 'AVQ_Microdati_2023.txt'), header = TRUE, sep = "\t")

##############################################
# drop rows for underage participants (1 to 6)
##############################################
all_dat = all_dat[all_dat$ETAMi>=7, ]

# select variables of interest 
my_vars_dat = read_excel('Docs/var_list.xlsx')
my_vars = my_vars_dat$label

#dat <- all_dat[, my_vars, drop = FALSE] # keep only columns listed in my_vars

##############################################
# FIND OUT WHICH VARS HAVE >30% NAs
##############################################
colSums(is.na(all_dat[, my_vars, drop = FALSE]))
percentage_NAs_beforetrans = colSums(is.na(all_dat[, my_vars, drop = FALSE])) / nrow(all_dat[, my_vars, drop = FALSE]) * 100
# find out which variables have > 30% NAs
NA_vars_beforetrans = percentage_NAs_beforetrans[percentage_NAs_beforetrans>30] 
names(NA_vars_beforetrans) 

##############################################
# RESTRUCTURE SOME VARS based on my_vars_dat: replace NAs with 
# 1) select vars needing restructuring based on !is.na(my_vars_dat$NA_strategy)
# for each var needing restructuring:
# 2) select the linked var, its value (or values) and the replace_NA_with value
# 3) replace the NA with the replace_NA_with value
##############################################

restruct_vars = my_vars_dat[!is.na(my_vars_dat$NA_strategy), ]$label
setdiff(names(NA_vars_beforetrans) , restruct_vars) # dovrebbe essere solo AMATR

for (i in restruct_vars){
  
  linked_var = my_vars_dat[my_vars_dat$label==i, ]$linked_var
  linked_var_value = my_vars_dat[my_vars_dat$label==i, ]$linked_var_value
  replace_NA_with = my_vars_dat[my_vars_dat$label==i, ]$replace_NA_with
  #all_dat[all_dat[linked_var]==linked_var_value, ][i] = replace_NA_with
  
  linked_vals  <- as.numeric(strsplit(linked_var_value, ",")[[1]])
  replace_vals <- as.numeric(strsplit(replace_NA_with, ",")[[1]])
  
  na_idx <- is.na(all_dat[[i]])
  
  if (i == "POSIZMi") {
    for (k in seq_along(linked_vals)) {
      idx <- all_dat[[linked_var]] == linked_vals[k] & na_idx
      all_dat[[i]][idx] <- replace_vals[k]
    }
    
  } else {
    idx <- if (length(linked_vals) > 1) {
      all_dat[[linked_var]] %in% linked_vals & na_idx
    } else {
      all_dat[[linked_var]] == linked_vals & na_idx
    }
    all_dat[[i]][idx] <- replace_NA_with # replace
  }

}

# something changed
percentage_NAs_aftertrans = colSums(is.na(all_dat[, my_vars, drop = FALSE])) / nrow(all_dat[, my_vars, drop = FALSE]) * 100
# find out which variables have > 30% NAs
percentage_NAs_aftertrans = percentage_NAs_aftertrans[percentage_NAs_aftertrans>10] 
names(percentage_NAs_aftertrans) 

##############################################
# keep only columns listed in my_vars
##############################################
dat <- all_dat[, my_vars, drop = FALSE] 

# concatenate PROFAM and PROIND to get participant ID
dat$ID = paste0(dat$PROFAM, "_", dat$PROIND)
# drop them from vars of interest
vars_interest = my_vars[3:length(my_vars)]
# drop them from database
dat = dat[, !names(dat) %in% c('PROFAM', 'PROIND')]

percentage_NAs_dat = colSums(is.na(dat)) / nrow(dat) * 100
# find out which variables have > 30% NAs
percentage_NAs_dat = percentage_NAs_dat[percentage_NAs_dat>30] 
names(percentage_NAs_dat)

##############################################
# HANDLE DON'T KNOW/NOT AVAILABLE DAT: replace unavailable data (99 and 9) with NAs
##############################################

# 99 corresponds to 'don't know/not available'. count how many there are and decide whether to remove these lines or substitute them with NAs so that we can impute them later
res99 = colSums(dat==99, na.rm = T)
res99[res99>0]

percentage_99s = res99 / nrow(dat) * 100
# find out which variables have > 30% 99
vars99 = percentage_99s[percentage_99s>30] 
#vars99 = names(vars99) 

# ISTRMi POSIZMi   HHRAD   HHTEL.
# ISTRMi: titolo di studio: 01	laurea e post-laurea; 07	diploma; 09	licenza di scuola media; 10	licenza di scuola elementare, nessun titolo di studio; 99	non disponibile.
# POSIZMi: posizione lavorativa. 01	dirigente; autonomo come imprenditore; libero professionista; 02	direttivo, quadro; impiegato; 03	capo operaio, operaio subalterno e assimilati; apprendista; lavorante a domicilio per conto d'impresa; 04	lavoratore in proprio; socio cooperativa Produzione Beni e/o prestazioni di servizio; coadiuvante; collaborazione coordinata e continuativa (con o senza progetto); prestazione d'opera occasionale; 99	non disponibile
# leave ISTRMi and POSIZMi as they are
# HHTEL has > 30% 99s, drop? 

# in CONDMi,STCIVMi and CITTMi 9=not available.
res9 = colSums(dat==9, na.rm = T)
res9[res9>0]   

percentage_9s = res9 / nrow(dat) * 100
# find out which variables have > 30% 99
vars9 = percentage_9s[percentage_9s>10] 

# replace unavailable data (99 and 9) with NAs
dat_replaced = dat # copy dat so that we can see how many NAS more the variables have after substituting "not availables" with NAs
cols <- c("ISTRMi", "POSIZMi", "HHRAD", "HHTEL")
dat_replaced[cols] <- lapply(dat_replaced[cols], function(x) replace(x, x == 99, NA))
cols2 = c('CONDMi', 'STCIVMi')
dat_replaced[cols2] <- lapply(dat_replaced[cols2], function(x) replace(x, x == 9, NA))

percentage_NAs_replaced = colSums(is.na(dat_replaced)) / nrow(dat_replaced) * 100
# find out which variables have > 20% NAs
percentage_NAs_replaced = percentage_NAs_replaced[percentage_NAs_replaced>20] 
names(percentage_NAs_replaced)

##############################################
# HOW MANY COMPLETE CASES ARE THERE?
##############################################

prova <- dat_replaced[complete.cases(dat_replaced), ] # 1984 complete observations, including amatr sodlav etc (all these vars that only select
# married and working people)

dat_replaced_vars_interest = dat_replaced[, !names(dat_replaced) %in% c('AMATR', 'SODLAV2')]
dat_replaced_vars_interest = dat_replaced_vars_interest[complete.cases(dat_replaced_vars_interest), ] # 4263 complete observations if we drop those vars with >50% NAs
# (keeping who does not have a marr year and who is presumably not working)

dat_replaced_vars_interest2 = dat_replaced[, !names(dat_replaced) %in% names(percentage_NAs_replaced)]
dat_replaced_vars_interest2 = dat_replaced_vars_interest2[complete.cases(dat_replaced_vars_interest2), ] # 22936 complete cases when dropping
# those variables >20% NAs

setdiff(colnames(dat_replaced_vars_interest), colnames(dat_replaced_vars_interest2))

colnames(dat_replaced_vars_interest2)

##############################################
# CREATE AGGREGATED VARS
##############################################

mydat = dat_replaced_vars_interest2[! names(dat_replaced_vars_interest2) %in% c('ID', 'CITTMi')]

# make sure that yes/no vars have the same values
y_n_vars = c("PSIND", "PGRVO", "PCULT", "PASPRO", "VOLON")

for (i in y_n_vars){
  if (i=='PSIND'|i=='PCULT'|i=='VOLON'){
    
    mydat[[i]] <- ifelse(mydat[[i]] == 3, 1,
                          ifelse(mydat[[i]] == 4, 2, mydat[[i]]))
    
  } else {
    
    mydat[[i]] <- ifelse(mydat[[i]] == 5, 1,
                         ifelse(mydat[[i]] == 6, 2, mydat[[i]]))
  }
}

save(mydat, file=paste0(export_folder, 'dat_completecases_before_aggr.RData'))
save(dat_replaced, file=paste0(export_folder, 'dat_with_NAs_before_aggr.RData'))

vars = NULL

for (i in names(mydat)[! names(mydat) %in% c('ID')]){
  
  curr_var = NULL
  curr_var$values = unique(dat_replaced_vars_interest2[[i]])
  curr_var$var = i
  curr_var = as.data.frame(curr_var)
  vars = rbind(curr_var, vars)
}
write.table(vars, file=paste0(export_folder, 'vars_values_before_aggr.csv'), sep = ' ', na = "NA", dec = ",", col.names = T, row.names = F)

# get table with ranges and quantiles
tab_vars = NULL

for (i in names(mydat)[! names(mydat) %in% c('ID')]){
  
  tab <- data.frame(matrix(ncol = 5, nrow = 1))
  colnames(tab) <- names(quantile(as.numeric(mydat[[i]]), na.rm = T))
  tab[1,] = quantile(as.numeric(mydat[[i]]), na.rm = T)
  tab$var = i
  
  tab_vars = rbind(tab_vars, tab)
}

write.table(tab_vars, file=paste0(export_folder, 'vars_ranges_before_aggr.csv'), sep = ' ', na = "NA", dec = ",", col.names = T, row.names = F)

# create aggregated vars based on Docs/var_list.xlsx
# aggregate: find vars to aggregate
my_vars_dat = read_excel('Docs/var_list.xlsx')
vars_to_aggr = my_vars_dat %>%
  filter(notes == "aggregare") %>%
  dplyr::select(label, new_label) %>%
  as.data.frame()

# make sure all vars are numeric
char_cols <- sapply(mydat, is.character)
mydat_char = colnames(mydat[, char_cols])
fact_cols = sapply(mydat, is.factor)
colnames(mydat[, fact_cols]) # there are no factors

for (i in mydat_char){
  mydat[[i]] = as.numeric(mydat[[i]])
}

char_cols <- sapply(mydat, is.character)
colnames(mydat[, char_cols]) # no more chars

deleted_labs = setdiff(vars_to_aggr$label, colnames(mydat))
vars_to_aggr <- vars_to_aggr[!(vars_to_aggr$label %in% deleted_labs), ]

for (new_var in unique(vars_to_aggr$new_label)){
  cols_to_aggr = vars_to_aggr[vars_to_aggr$new_label==new_var, ]$label
  
  mydat$aggregated <- rowMeans(mydat[, cols_to_aggr], na.rm = F)
  
  colnames(mydat)[colnames(mydat)=='aggregated'] = new_var
  
  mydat = mydat[, !names(mydat) %in% cols_to_aggr]
  
}

# check drop of redundant columns
vars_pre_aggr = names(mydat)[! names(mydat) %in% 'ID']
setdiff(vars_pre_aggr, colnames(mydat))
setdiff(colnames(mydat), vars_pre_aggr)
setdiff(unique(vars_to_aggr$new_label), colnames(mydat))

# get new table with ranges and quantiles for aggregated data
tab_all_aggr = NULL

for (i in names(mydat)[! names(mydat) %in% c('ID')]){
  
  tab <- data.frame(matrix(ncol = 5, nrow = 1))
  colnames(tab) <- names(quantile(as.numeric(mydat[[i]]), na.rm = T))
  tab[1,] = quantile(as.numeric(mydat[[i]]), na.rm = T)
  tab$var = i
  
  tab_all_aggr = rbind(tab_all_aggr, tab)
}

write.table(tab_all_aggr, file=paste0(export_folder, 'vars_ranges_aggr.csv'), sep = ' ', na = "NA", dec = ",", col.names = T, row.names = F)

mydat = round(mydat, digits=0)
save(mydat, file=paste0(export_folder, 'dat_completecases_aggr.RData'))

# write.csv(dat, file=paste0(export_folder, 'dat_99s_NAs_corrected.csv'), dec = ",", col.names = TRUE, row.names = F, sep=' ')

# inspect histograms

for(n in 1:ncol(mydat)){
  hist(mydat[,n], main = colnames(mydat)[n])
  invisible(readline(prompt="Press [enter] to continue"))
}
