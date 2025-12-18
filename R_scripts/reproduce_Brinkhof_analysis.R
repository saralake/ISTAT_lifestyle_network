rm(list=ls())

#R functions
library(readr); library(dplyr); library(reshape); library(reshape2); library(Rmisc)
library(doBy); library(car); library(Hmisc); library(psych); library(utils); library(plotrix)
library(readxl); options(contrasts=c("contr.sum","contr.poly")); library(afex); 
library(ramify); library(imputeTS); library(readxl); library(nparLD);   library(effsize)
library("qgraph"); library(igraph); library(bootnet); library(mgm); library("huge");
library(wesanderson); library(varhandle); library(stringr); library(RColorBrewer); library(mltools);
library(relaimpo); library("Rfast");library("writexl"); library(tictoc)

import_folder = 'Intermediate_data/ver2/'
export_folder = 'Results/reproduce_Brinkhof/ver1/'

if(!dir.exists(export_folder)){
  dir.create(export_folder)
}

load(paste0(import_folder, 'dat_completecases_aggr.RData'))

var_types = read_excel('Docs/var_types_new.xlsx')
groups = var_types$group
group_cols <- brewer.pal(n = length(unique(groups)), name = 'Set3')

str(mydat)
summary(mydat)

# frequency of behavior = count (Poisson) data; Likert = gaussian; factor= categorical
# fix binary variables 3, 7, 13, 31, 40, 54 (they have to be 0/1 for mgm to work)
vars_to_fix = colnames(mydat)[c(3, 13, 31, 40, 56)]
mydat$SESSO <- mydat$SESSO - 1 # 1 maschio 2 femmina -> 0 maschio 1 femmina
mydat$PARENT = mydat$PARENT -1 # 1 no 2 sì -> 0 no 1 sì
mydat$CRONI = mydat$CRONI - 1 # 1 no 2 sì -> 0 no 1 sì
mydat$INCSPI = mydat$INCSPI -1 # 1 no 2 sì -> 0 no 1 sì
mydat$SOC_LIFE_STRUTT_aggr = mydat$SOC_LIFE_STRUTT_aggr -1 # 1 no 2 sì -> 0 no 1 sì

DATA <- list()
DATA$data <- as.matrix(mydat)
DATA$colnames <- colnames(mydat)
DATA$names <- var_types$names
DATA$type <- var_types$type
DATA$level <- var_types$level
DATA$group = var_types$group

fit_mgm_defaultpar <- estimateNetwork(data = DATA$data, type=DATA$type, level=DATA$level, default = "mgm", parallel = TRUE, nCores = 6)

tic()
fit_mgm_Brinkpar <- estimateNetwork(data = DATA$data, type=DATA$type, level=DATA$level, 
                           default = "mgm", # estimateNetwork with default = mgm calls bootnet_mgm
                           tuning = 0.25, 
                           threshold = "LW",
                           rule = 'AND',
                           criterion = "EBIC", # instead of lambdaSel in mgm?
                           binarySign = TRUE,
                           # lambdaGam = 0.25,# this gives Error in mgm::mgm(data, verbatim = !verbose, warnings = verbose, signInfo = FALSE,  :formal argument "lambdaGam" matched by multiple actual arguments
                           # k = 2, # Error in mgm::mgm(data, verbatim = !verbose, warnings = verbose, signInfo = FALSE,  : formal argument "k" matched by multiple actual arguments
                           parallel = TRUE, nCores = 6) 
toc()
save(fit_mgm_Brinkpar, file=paste0(export_folder, 'estimateNetwork_mgm_Brinkpar.Rdata'))

tic()
mgm_objt <- mgm(data = DATA$data,
                type = DATA$type,
                level = DATA$level,
                lambdaSel = "EBIC",
                lambdaGam = 0.25,
                k = 2,
                rulereg = "AND",
                binarySign = TRUE,
                scale = TRUE,
                threshold = "LW",
                parallel=T, ncores=6)
toc()
save(mgm_objt, file=paste0(export_folder, 'mgm_objt.Rdata'))

weights_mat_mgm <- fit_mgm_defaultpar$graph
qgraph(weights_mat_mgm,
       labels = colnames(mydat), groups = DATA$group, edge.labels=T, layout='spring')

par(mfrow=c(1,2))

qgraph(input = weights_mat_mgm, title = "estimateNetwork DEFAULT PARS",
       nodeNames = DATA$colnames, layout = "spring", repulsion = 0.9,
       # edge.color = mgm_objt$pairwise$edgecolor, 
       color = alpha(group_cols, 0.5), vsize = 5, groups = DATA$group,
       details = FALSE, legend.mode = "style2",legend.cex = 0.3,
       label.font = 2,curveAll = T, curve = 0.7, minimum = 0.05, edge.width = 1)

qgraph(input = mgm_objt$pairwise$wadj, title = "mgm BRINKHOF CODE",
       nodeNames = DATA$colnames, layout = "spring", repulsion = 0.9,
       edge.color = mgm_objt$pairwise$edgecolor, 
       color = alpha(group_cols, 0.5), vsize = 5, groups = DATA$group,
       details = FALSE, legend.mode = "style2",legend.cex = 0.3,
       label.font = 2,curveAll = T, curve = 0.7, minimum = 0.05, edge.width = 1)

# NOW fit_mgm_defaultpar AND mgm_objt ARE THE SAME, BUt mgm_objt returns some grey edges: try bootstrapping fit_mgm_defaultpar

par(mfrow=c(1,1))
qgraph(input = fit_mgm_Brinkpar$graph, title = "estimateNetwork BRINKHOF pars", 
       nodeNames = DATA$colnames, layout = "spring", repulsion = 0.9,
       # edge.color = mgm_objt$pairwise$edgecolor, 
       color = alpha(group_cols, 0.5), vsize = 5, groups = DATA$group,
       details = FALSE, legend.mode = "style2",legend.cex = 0.3,
       label.font = 2,curveAll = T, curve = 0.7, minimum = 0.05, edge.width = 1)
# this is also equal to the previous 2


fit_mgm_bootstrapped = bootnet(fit_mgm)
png('mgm_net_bootstrapped.png')
plot(fit_mgm_bootstrapped$sample, groups = group, edge.labels=T, layout='spring')
dev.off()

qgraph(fit_mgm_bootstrapped$sample$graph, labels = colnames(dat), groups = group, edge.labels=T, layout='spring') # same as plot above



#Network Estimation: ===========================================================
#Create dataframe: -------------------------------------------------------------
DATA <- list()
DATA$data <- as.matrix(data_net30)
DATA$colnames <- colnames(data_net30)
DATA$names <- c("N_FAM", "AGE", "SEX", "MAR_STAT", "EDU", "WORKING", "WORKED", "JOB", "PAS", "BC", "PS", "MLE", "GSE", "SE", "SMA",
                "LONE", "SSD", "HEA", "PND", "SQ", "PHY", "MHD", "AU", "BORE", "PRM", "SI", "PNA", "PPA", "CON1", "SPoA")
DATA$type <- c("g", "g", "g", "g", "g", 
               "g","g", "g", "g", "g",
               "g", "g", "g", "g", "g",
               "g", "g", "g", "c","g",
               "g", "c", "g", "g", "g",
               "g","g", "g", "g", "g")
DATA$level <- c(1,1,1,1,1,1,1,1,1,1,
                1,1,1,1,1,1,1,1,8,
                1,1,3,1,1,1,
                1,1,1,1,1)

#Estimate Network: -------------------------------------------------------------

#EBIC regularization
set.seed(3)
mgm_objt <- mgm(data = DATA$data,
                type = DATA$type,
                level = DATA$level,
                lambdaSel = "EBIC",
                lambdaGam = 0.25,
                k = 2,
                rulereg = "AND",
                binarySign = TRUE,
                scale = TRUE,
                threshold = "LW")
# SL find out adjacency matrix
adj <- round(mgm_objt$pairwise$wadj,2)
adj <- cbind(" " = DATA$names, adj)
adj <- as.data.frame(adj); colnames(adj) <- c(NA, DATA$names) 
write_xlsx(adj,"mgm_objt_adj_matrixApr2024.xlsx")

write_xlsx(adj,"check_matrixApr2024.xlsx")

#Plot raw network: -------------------------------------------------------------
group_list <- list("Demographics" = c(1,2), "(Mental) Health" = c(3,4,5,6,7,16,17,18,19,22,24), "Coping/Personality" = c(8,9,10,13,14,15), 
                   "Stress" = c(11,12), "Lifestyle" = c(20,21,23,25,29), "Ageism" = c(30,27,28),  "COVID" = c(26))
group_cols <- brewer.pal(n = 11, name = 'Set3')

library(scales)
par(mfrow=c(1,1))
mod <- qgraph(input = mgm_objt$pairwise$wadj, title = "Mixed Graphical Model",
              nodeNames = DATA$names, layout = "spring", repulsion = 0.9,
              edge.color = mgm_objt$pairwise$edgecolor, #edge.color = ifelse(mgm_obj$pairwise$edgecolor == "darkgreen", "blue", mgm_obj$pairwise$edgecolor),
              color = alpha(group_cols, 0.5),vsize = 5, groups = group_list,
              details = FALSE, legend.mode = "style2",legend.cex = 0.3,
              label.font = 2,curveAll = T, curve = 0.7, minimum = 0.05, edge.width = 1)

hist(mgm_objt$pairwise$wadj[mgm_objt$pairwise$wadj > 0], 30,xlab = "Edge weight", main = " ",
     ylim = c(0,25))
abline(v = 0.05, lty = 2)