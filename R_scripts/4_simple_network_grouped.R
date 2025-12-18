# simple network estimation. with mgm and walktrap.
rm(list=ls())

library(psych)
library(psychonetrics)
library(dplyr)
library(qgraph)
library(mgm)
library(bootnet)
library(igraph)

select <- dplyr::select
filter <- dplyr::filter

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

#sort_by_loadings(loadings)

#loadings_red = loadings_clean[loadings_clean$dominant_factor %in% c("ML1", "ML10", "ML5", "ML6"), ]
#loadings_red = loadings_red[ colnames(loadings_red) %in% c("ML1", "ML10", "ML5", "ML6", 'dominant_factor')]

# remove vars with small loadings from train_dat
removed_vars = setdiff(colnames(train_dat), rownames(loadings_clean))
train_dat = train_dat[ ! colnames(train_dat) %in% removed_vars]
dat = train_dat[ ! colnames(train_dat) %in% 'SESSO']

sum(is.na(dat))

# ESTIMATE NETWORK
fit_mgm <- estimateNetwork(data = dat, default = "mgm")
weights_mat_mgm <- fit_mgm$graph
png(paste0(export_folder, 'mgm_net.png'), width = 2000, height = 2000)
qgraph(weights_mat_mgm,
       labels = colnames(dat), edge.labels=F, layout='spring') #groups = group, 
dev.off()

# BOOTSTRAP to obtain sparser network
fit_mgm_bootstrapped = bootnet(fit_mgm)
save(fit_mgm_bootstrapped, file=paste0(export_folder, "mgm_bootstrapped_network_mod.RData"))
png(paste0(export_folder, 'mgm_net_bootstrapped.png'), width = 2000, height = 2000)
qgraph(fit_mgm_bootstrapped$sample$graph, 
     labels=colnames(dat), edge.labels=F, layout='spring') # groups = group, 
dev.off()

# extract weight matrix
mgm_boot_net = fit_mgm_bootstrapped$sample$graph

# convert the network to igraph
network <- graph_from_adjacency_matrix(abs(mgm_boot_net), weighted = TRUE, mode = "undirected", diag = FALSE)

# Apply the Walktrap clustering algorithm
walktrap_clusters <- cluster_walktrap(network, steps = 4)

# define a membership vector
membership <- factor(walktrap_clusters$membership, levels = unique(walktrap_clusters$membership), 
                     labels = paste('factor_', unique(walktrap_clusters$membership), sep=''))
# visualize the network with its walktrap community structure
qgraph(mgm_boot_net, layout = "spring", groups = membership)


# reduce to small, understandable dataset 

names <- colnames(train_dat)

longnames <- c("alcol_fuoripasto",
               "alcol_unità_eccesso",
               "salute_opinione",
               "cronic_illness",
               "health_limitations",
               "SF9",
               "SF11",
               "SF13",
               "SF14",
               "SF15",
               "alcol_unità_abituali",
               "alcol_unità_giorno")

clusters <- list("ALCOL"=c(1:2, 11:12),
                 "SALUTE"=c(3:5), 
                 "PSICO"=c(6:10))

## Estimate network 
DelayedResults <- estimateNetwork(train_dat, default="EBICglasso")

DelayedResults$graph #weights matrix
## write.csv(DelayedResults$graph, "WeightsMatrixDelayed.csv")

## Plot the network
Network_Plot<-plot(DelayedResults, layout="spring", labels=names, vsize=6, cut=0,
                   border.width=1.8, border.color="black", groups=clusters,
                   color=c("#98fb98", "#87cefa", "#ff66cc"),
                   nodeNames=longnames, legend.cex=.6, negDashed=T)

## With caption
DelayedResults_plot<-plot(DelayedResults, layout="spring", labels=names, vsize=5, cut=0, 
                          border.width=1.8, border.color="black",groups=clusters,
                          color=c("#98fb98", "#87cefa", "#ff66cc"),
                          nodeNames = longnames,legend.cex=.45, negDashed=TRUE,
                          title="Delayed sample network N=364", title.cex=1.3,
                          #filename = "DelayedNetworkWithCaption", filetype = "pdf", width=14, height=10
                          )
