# libraries
setwd("~/Pessoa_Lab/MAX")
library(rstan)
library(parallel)
library(dplyr)

print(getOption("mc.cores"))
options(mc.cores = parallel::detectCores())
print(getOption("mc.cores"))

# read in data
dataset = read.csv("MAX_neutral_early_late.txt", header = TRUE,sep = ',')
dataset <-filter(dataset,Phase=='early')
dataset$se = sqrt(dataset$var)

dataset <- select(dataset,Subj,ROI,beta,se,TvsS,TRAIT,STATE)
colnames(dataset)[5] <- "cond"

dataset$ROI <- factor(dataset$ROI)
dataset$Subj <- factor(dataset$Subj)

print(paste("Number of Subjects:-",nlevels(dataset$Subj)))
print(paste("Number of ROIs:-",nlevels(dataset$ROI)))
print(paste0('Number of cores available: ', detectCores(all.tests = FALSE, logical = TRUE)))

# processing 
num_subjs <- nlevels(dataset$Subj)
# set values of subject to index mapping
subj_to_idx <- 1 : num_subjs 
# set indices of the mapping
names(subj_to_idx) <- levels(dataset$Subj)
idx_to_subj <- levels(dataset$Subj)
names(idx_to_subj) <- 1 : num_subjs

num_rois <- nlevels(dataset$ROI) 
roi_to_idx <- 1 : num_rois
names(roi_to_idx) <- levels(dataset$ROI)
idx_to_roi <- levels(dataset$ROI)
names(idx_to_roi) <- 1 : num_rois

# format data for Stan:
model_data <- list(N = nrow(dataset),
                   Y = dataset$beta,
                   se = dataset$se,
                   cond = dataset$cond,
                   trait = dataset$TRAIT,
                   state = dataset$STATE,
                   N_SUB = num_subjs,
                   N_ROI = num_rois,
                   sid = subj_to_idx[dataset$Subj],
                   rid = roi_to_idx[dataset$ROI])

# set Stan model
model <- stan_model(file = "03-MAX-model1.stan")
##########################################################################################################
# Sample from posterior distribution:
time0 = system.time(model_fit <- sampling(object = model, 
                                          data = model_data, 
                                          iter = 50000, 
                                          chains = 4, 
                                          cores = 4,
                                          control = list(adapt_delta = 0.99, max_treedepth = 15)))

print(time0)

save.image(file="03-MAX-model1.RData")

