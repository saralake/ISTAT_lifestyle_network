# this step on the training dataset carries out an explorative factor analysis in said dataset and helps us understand which items to drop

rm(list=ls())

library(corrplot)
library(psych)

# FACTOR ANALYSIS ON TRAIN DATA to discover factor structure

import_folder = 'Intermediate_data/ver1/'
export_folder = 'Results/training_set_factor_strucure/ver1/'

if(!dir.exists(export_folder)){
  dir.create(export_folder)
}

source('R_functions/get_fa_residuals.R')
source('R_functions/sort_by_loadings.R')

###################################################################
# REMOVE UNAVAILABLE DATA IMPOSSIBLE TO RECONSTRUCT WITH IMPUTATION
###################################################################

train_dat = read.csv(paste0(import_folder, 'train_dat_imputed_aggregated.csv'), sep = '', dec=',')

percentage_NAs = colSums(is.na(train_dat)) / nrow(train_dat) * 100
percentage_NAs[percentage_NAs>0] # AMATR   LAVPAS  POSIZMi    TLADO     TLAV  SODLAV2 MARR_YEARS

# in CONDMi,STCIVMi and CITTMi 9=not available.
# in ISTRMi and POSIZMi 99=not available. Exclude those rows where these variables take these values (they were excluded from the imputation because of this)

res99 = colSums(train_dat==99, na.rm = T)
res99[res99>0]

train_dat <- train_dat[
  (train_dat$ISTRMi != 99 | is.na(train_dat$ISTRMi)) &
    (train_dat$POSIZMi != 99 | is.na(train_dat$POSIZMi)) &
    (train_dat$CONDMi  !=  9 | is.na(train_dat$CONDMi))  &
    (train_dat$STCIVMi !=  9 | is.na(train_dat$STCIVMi)) &
    (train_dat$CITTMi  !=  9 | is.na(train_dat$CITTMi)), 
]

percentage_NAs = colSums(is.na(train_dat)) / nrow(train_dat) * 100
percentage_NAs[percentage_NAs>0]

# keep only vars
train_dat_sel = train_dat[, !names(train_dat) %in% c("AMATR", 'ID', "LAVPAS", "POSIZMi", "TLADO", "TLAV", "SODLAV2", "MARR_YEARS", 'MH')] # elimino anche LAVPAS  POSIZMi    TLADO     TLAV  
# SODLAV2 perchè sono relate alla posizione lavorativa. Stessa cosa per MARR_YEARS, riferito solo a persone sposate. se esce positivo qualcosa della posizione lavorativa, inseriamo 

# condmi = posizione professionale. 1	occupato 2	in cerca di occupazione	3	inattivo

# OPTIONAL: exclude sex
train_dat_sel = train_dat_sel[, !names(train_dat_sel) %in% c("SESSO")] 

png(paste0(export_folder, 'corrplot.png'), height = 2000, width = 2000, pointsize = 14)
corrplot(cor(train_dat_sel), method = "number", type = 'upper')
dev.off()

cortest.bartlett(train_dat_sel) # p<0.05: go on with analysis

train_dat_sel = train_dat_sel[, !names(train_dat_sel) %in% c("MH")] # escludo MH altrimenti mi abbassa il MSAi di tutte le variabili psico

# KMO TEST
res = KMO(train_dat_sel)
kmo = res$MSAi
res$MSAi[res$MSAi<0.6] # few vars below 0.6 

train_dat_sel <- train_dat_sel[, KMO(train_dat_sel)$MSAi>0.50] # Get rid of all variables with MSA < 0.50, but there are none so ok

# given the large sample, we can either use Kaiser's criterion (retain factors with eigenvalues >1) or the scree plot criterion 
# (see section 17.3.8. Factor extraction: eigenvalues and the scree plot in Discovering Statistics).
# we will inspect the residuals to decide wich option has a better model fit

ev <- eigen(cor(train_dat_sel)) # get eigenvalues. 20 are >1
ev$values

fa.parallel(train_dat_sel, fa="fa") # Parallel analysis suggests that the number of factors =  19  and the number of components =  NA 

scree(train_dat_sel, pc=F) # this justifies 5 factors (do not consider the point of inflection). 

fa_res15 = fa(train_dat_sel, nfactors=15, fm = "ml") # Kaiser's criterion: 13 factors have eigenvalues (SS loadings)>1.

fa_scree = fa(train_dat_sel, nfactors = 5, fm='ml', rotate = 'none') # fit 0.83
fa_kaiser = fa(train_dat_sel, nfactors = 13, fm='ml', rotate = 'none') # more factors better fit. fit is good (0.96) but residuals not really

fa_res10 = fa(train_dat_sel, nfactors = 10, fm='ml', rotate = 'none')

# get the reproduced correlation matrix. The diagonal of this matrix contains the communalities after extraction for each variable
factor.model(fa_scree$loadings) 
factor.model(fa_kaiser$loadings)

# get residuals, calculated as the difference between the reproduced correlation matrix and the correlation matrix in the data.
factor.residuals(train_dat_sel, fa_scree$loadings) # The diagonal of this matrix is the uniquenesses.
factor.residuals(train_dat_sel, fa_kaiser$loadings)

fa_scree_oblimin = fa(train_dat_sel, nfactors = 5, fm='ml', rotate = 'oblimin')
fa_kaiser_oblimin = fa(train_dat_sel, nfactors = 13, fm='ml', rotate = 'oblimin')

fa_res10_oblimin = fa(train_dat_sel, nfactors = 10, fm='ml', rotate = 'oblimin')

fa_kaiser_promax = fa(train_dat_sel, nfactors = 13, fm='ml', rotate = 'promax')

get_fa_residuals(factor.residuals(train_dat_sel, fa_scree_oblimin$loadings))
get_fa_residuals(factor.residuals(train_dat_sel, fa_kaiser_oblimin$loadings)) # still better residuals than promax
get_fa_residuals(factor.residuals(train_dat_sel, fa_kaiser_promax$loadings))

get_fa_residuals(factor.residuals(train_dat_sel, fa_res10_oblimin$loadings))

sink(file = paste0(export_folder, 'factor_anal_not_rotated.txt'))
print.psych(fa_kaiser, cut = 0.3, sort = TRUE) 
sink()
sink(file = paste0(export_folder, 'factor_anal_oblimin.txt'))
print.psych(fa_kaiser_oblimin, cut = 0.3, sort = TRUE) # rotated or not, they have equivalent statistics
sink()
sink(file = paste0(export_folder, 'factor_anal_promax.txt'))
print.psych(fa_kaiser_promax, cut = 0.3, sort = T)
sink()
sink(file = paste0(export_folder, 'factor_anal_oblimin_10factors.txt'))
print.psych(fa_res10, cut = 0.3, sort = TRUE) # rotated or not, they have equivalent statistics
sink()

# items load more neatly if oblimin, so go on with that

# get the structure matrix: takes into account the relationship between factors 
# (it is a product of the pattern matrix, $loadings, and the matrix containing the correlation coefficients between factors, $Phi).
fa_kaiser_obli_structuremat = fa_kaiser_oblimin$loadings %*% fa_kaiser_oblimin$Phi # structure matrix: 

#my_factors = colnames(fa_kaiser_obli_structuremat)
#items = rownames(fa_kaiser_obli_structuremat)

source('R_functions/sort_by_loadings.R')

sink(file = paste0(export_folder, 'loadings_tab_not_rotated.txt'))
sort_by_loadings(fa_kaiser$loadings[,], print.cut = 0.3)
sink()
sink(file = paste0(export_folder, 'loadings_tab_oblimin.txt'))
sort_by_loadings(fa_kaiser_oblimin$loadings[,], print.cut = 0.3)
sink()
sink(file = paste0(export_folder, 'loadings_tab_promax.txt'))
sort_by_loadings(fa_kaiser_promax$loadings[,], print.cut = 0.3)
sink()

sort_by_loadings(fa_kaiser_oblimin$loadings[,])
sort_by_loadings(fa_kaiser_obli_structuremat)
save(fa_kaiser_obli_structuremat, file=paste0(export_folder, 'factor_structure_matrix.RData'))

loadings = fa_kaiser_oblimin$loadings[,]
save(loadings, file=paste0(export_folder, 'factor_loadings.RData'))
write.table(sort_by_loadings(fa_kaiser_obli_structuremat), file=paste0(export_folder, 'sorted_factor_structure_matrix.csv'), sep = ' ', na = "NA", dec = ",", col.names = T, row.names = T)
write.table(sort_by_loadings(fa_kaiser_oblimin$loadings[,]), file=paste0(export_folder, 'sorted_factor_loadings.csv'), sep = ' ', na = "NA", dec = ",", col.names = T, row.names = T)


png(paste0(export_folder, 'fa_kaiser_obli_diagram_structuremat.png'), height = 1500)
fa.diagram(fa_kaiser_obli_structuremat)
dev.off()

png(paste0(export_folder, 'fa_kaiser_obli_diagram_loadings.png'), height = 1500)
fa.diagram(fa_kaiser_oblimin$loadings)
dev.off()

print(fa_kaiser_oblimin, digits=2, cutoff=0.3, sort=TRUE)

fa_kaiser_oblimin_scores = fa_kaiser_oblimin$scores # scores per factor and participant. 
save(fa_kaiser_oblimin_scores, file=paste0(export_folder, 'factor_scores.RData'))

load <- fa_kaiser_oblimin$loadings[,4:5]
plot(load,type="n") # set up plot
text(load,labels=names(train_dat_sel),cex=.7)

factor_corr_coefficients = fa_kaiser_oblimin$Phi
save(factor_corr_coefficients, file=paste0(export_folder, 'corr_coefficients_between_factors_trainingdat.RData'))

# save clean train dataset
#save(train_dat, file='Intermediate_data/train_dat_clean.RData')
