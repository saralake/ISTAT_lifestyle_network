# paper https://doi.org/10.1002/jts.22988
rm(list = ls())

library(foreign)
library(qgraph)
library(igraph)
library(bootnet)
library(ggplot2)
library(networktools)     
library(ltm)
library(MASS)
library(msm)
library(polycor)
library(NetworkComparisonTest)

sessionInfo()

all_dat = read.spss('C19PRC_UKW1W2_archive_final.sav', to.data.frame = T, use.value.labels = F)

GAD_dat = all_dat[, grep(paste('W1_GAD_'), names(all_dat)), drop = FALSE]
GAD_dat = GAD_dat[, colnames(GAD_dat)[c(1:7)]]
PHQ_dat = all_dat[, grep(paste('W1_PTSD'), names(all_dat)), drop = FALSE]
PHQ_dat = PHQ_dat[, colnames(PHQ_dat)[c(1:6)]]
COVID_dat = all_dat[, grep(paste('W1_COVID19_anxiety'), names(all_dat)), drop = FALSE] # W1_COVID19_anxiety

dat = cbind(PHQ_dat, GAD_dat, COVID_dat)

colnames(dat)

#Names and labels
#----------------
names <- c("PTSD1", "PTSD2", "PTSD3", "PTSD4", "PTSD5", "PTSD6", 
           "GAD_1", "GAD_2", "GAD_3","GAD_4","GAD_5","GAD_6","GAD_7", 
           "COVID19")

longnames <- c("Interest",
               "Depressed",
               "Sleep",
               "Energy",
               "Eating",
               "Self-Image",
               "Nervous",
               "Worrying",
               "Generalized worrying",
               "Relaxing",
               "Restlessness",
               "Irritability",
               "Negative future",
               "Covid-19 Anxiety")

clusters <- list("PTSD-9"=c(1:6),
                 "GAD-7"=c(7:13), 
                 "COVID-19"=c(14))

colnames(dat) = names

## Estimate network 
DelayedResults <- estimateNetwork(dat, default="EBICglasso", threshold = TRUE)

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
                          filename = "DelayedNetworkWithCaption", filetype = "pdf", width=14, height=10)

# prova CFA
library(psychonetrics)

loadmat = matrix(0, nrow = length(names), ncol = 3)
loadmat[c(1:6),1] = 1
loadmat[c(7:13),2] = 1
loadmat[14,3] = 1

cfamod <- lvm(dat, lambda = loadmat, 
              identification = "variance",   
              estimator = "FIML",       
              latents = names(clusters))
cfamod <- cfamod %>% runmodel

cfamod %>% fit

cfamod@fitmeasures[c("chisq","pvalue","rmsea","cfi")]

cfamod %>% parameters

cfamod %>% MIs

lnmmod <- lnm(dat, 
              lambda = loadmat,
              identification = "variance", 
              estimator = "FIML",              
              latents = names(clusters))

# Run model by first pruning non significant edges and then automatically add edges at α=0.05 until BIC can no longer be improved.
lnmmod <- lnmmod %>% runmodel %>%  
  prune(adjust = "fdr", 
        alpha = 0.01) %>%   
  stepup(criterion = "bic", 
         alpha = 0.05)

lnmmod %>% parameters
lnmmod %>% fit
lnmmod@fitmeasures[c("chisq","pvalue","rmsea","cfi")]

lnmmod %>% MIs

lnmmod_free <- lnmmod %>% 
  freepar("lambda","PTSD6", 'GAD-7') %>% 
  runmodel

compare(orig = lnmmod, freed=lnmmod_free)

lnmmod_free %>% MIs

lnmmod_free@fitmeasures[c("chisq","pvalue","rmsea","cfi")]

lnmmod_free1 <- lnmmod_free %>% 
  freepar("lambda","PTSD5", 'GAD-7') %>% 
  runmodel

lnmmod_free1 %>% MIs

compare(orig = lnmmod, freed=lnmmod_free, freed1=lnmmod_free1, cfa=cfamod)

lnmmod_free1@fitmeasures[c("chisq","pvalue","rmsea","cfi")] # CFI > .90 = acceptable fit; RMSEA < .08 = acceptable

# 7. Plot LNM and factor loadings and/or plot them in one graph
# Obtain the edges between latent variables
lnmmod_edges <- getmatrix(lnmmod_free1, "omega_zeta")
# Obtain factor loadings
lnmmod_factorloadings <- getmatrix(lnmmod_free1, "lambda")
# Plot the latent network model
layout(t(1:2))
qgraph(lnmmod_edges, theme = "colorblind", layout = "spring",       
       title = "Latent network", vsize = 8, edge.labels = TRUE)
qgraph.loadings(lnmmod_factorloadings, model = "reflective",  
                title = "Factor loadings", vsize = c(8,13), asize = 5)

# residual network model
rnmmod <- rnm(dat, lambda = loadmat,
              estimator = "FIML") %>% runmodel

RNM <- rnmmod %>% stepup(criterion = "bic",
                         verbose = FALSE) %>% modelsearch(verbose = FALSE)

# Obtain residual network
residnet <- getmatrix(RNM, "omega_epsilon")
# Obtain factor loadings
factorloadingsR <- getmatrix(RNM, "lambda")
# Obtain correlations
factorCors <- getmatrix(RNM, "sigma_zeta")


#Plot the residual network model
layout(t(1:2))
par(mfrow = c(1, 2))  

qgraph(residnet, theme = "colorblind", layout = "spring",       
       title = "Residual network", vsize = 8)

qgraph.loadings(factorloadingsR, theme = "colorblind", model = "reflective", 
                title = "Factor loadings", vsize = c(5,8), asize = 3,
                factorCors = factorCors)

compare(orig = lnmmod, freed=lnmmod_free, freed1=lnmmod_free1, resid_net=RNM) # keep on freeing parameters until residual is no more better

lnmmod_free1 %>% MIs

lnmmod_free2 <- lnmmod_free1 %>% 
  freepar("lambda","GAD_5", 'PTSD-9') %>% 
  runmodel

compare(orig = lnmmod, freed=lnmmod_free, freed1=lnmmod_free1, freed2=lnmmod_free2, resid_net=RNM) # keep on freeing parameters until residual is no more better

lnmmod_free2 %>% MIs

lnmmod_free3 <- lnmmod_free2 %>% 
  freepar("lambda","GAD_7", 'COVID-19') %>% 
  runmodel

compare(orig = lnmmod, freed=lnmmod_free, freed1=lnmmod_free1, freed2=lnmmod_free2, free3=lnmmod_free3, resid_net=RNM) # keep on freeing parameters until residual is no more better

lnmmod_free3 %>% MIs

lnmmod_free4 <- lnmmod_free3%>% 
  freepar("lambda","PTSD5", 'COVID-19') %>% 
  runmodel

compare(orig = lnmmod, freed=lnmmod_free, freed1=lnmmod_free1, freed2=lnmmod_free2, free3=lnmmod_free3,
        free4=lnmmod_free4, resid_net=RNM) # keep on freeing parameters until residual is no more better

lnmmod_free4 %>% MIs

lnmmod_free5 <- lnmmod_free4 %>% 
  freepar("lambda","GAD_6", 'PTSD-9') %>% 
  runmodel

compare(orig = lnmmod, freed=lnmmod_free, freed1=lnmmod_free1, freed2=lnmmod_free2, free3=lnmmod_free3,
        free4=lnmmod_free4, free5=lnmmod_free5, resid_net=RNM) # keep on freeing parameters until residual is no more better

# 7. Plot LNM and factor loadings and/or plot them in one graph
# Obtain the edges between latent variables
lnmmod_edges <- getmatrix(lnmmod_free5, "omega_zeta")
# Obtain factor loadings
lnmmod_factorloadings <- getmatrix(lnmmod_free5, "lambda")
# Plot the latent network model
layout(t(1:2))
qgraph(lnmmod_edges, theme = "colorblind", layout = "spring",       
       title = "Latent network", vsize = 8, edge.labels = TRUE)
qgraph.loadings(lnmmod_factorloadings, model = "reflective",  
                title = "Factor loadings", vsize = c(8,13), asize = 5)

