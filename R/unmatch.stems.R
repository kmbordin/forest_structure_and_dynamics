############################################################
##############  CARBON ESTIMATES  ##########################
############################################################
### Code updated by Kauane M Bordin at 02 Oct 2025

### Description: Datasets sometimes may show some inconsistencies on treeids -
# they are not unique as they should be, which impacts the estimate of carbon 
# gains due to growth of survivors.

### In this code we can filter repeated treeids to match the same treeids and calculate 
# the carbon gains due growth of survivors. 
# Requires the dataset os survivors, metric='carbon' derived from demography function. 

# load packages ---------
library(tidyverse)

unmatch.stems <- function(data.survival.carbon){
  
  #' @description corrects for duplicate treeids
  #' @author Kauane Maiara Bordin
  #' @param data dataframe with stem level data of survivors
  #' @return dataframe with matching survivors between censuses
  #' data.survival.carbon: data of survivors derived from demography function, metric='carbon'
  
  survc <- data.survival.carbon
  surv_c1 <- survc %>% filter(census.n == 1) %>% arrange(treeid)
  surv_c2  <-  survc %>% filter(census.n == 2) %>% arrange(treeid)
  
  n_occur.surv1 <- surv_c1 %>% 
    group_by(treeid) %>% 
    summarise(Freq = n()) %>% 
    filter(Freq > 1) # counts and filters repeated treeids in census1
  n_occur.surv2 <- surv_c2 %>% 
    group_by(treeid) %>% 
    summarise(Freq = n()) %>% 
    filter(Freq > 1) # counts and filters repeated treeids in census2
  
  # removing duplicated treeids
  surv_c1 <- surv_c1 %>% 
    filter(! treeid %in% n_occur.surv1$treeid)
  surv_c2 <- surv_c2 %>% 
    filter(! treeid %in% n_occur.surv1$treeid)
  surv_c1 <- surv_c1 %>% 
    filter(! treeid %in% n_occur.surv2$treeid)
  surv_c2 <- surv_c2 %>% 
    filter(! treeid %in% n_occur.surv2$treeid)
  
  survc <- bind_rows(surv_c1,surv_c2)
  return(survc)
}

# usage: ------------------------------------------------------------------ 
# survival.carbon <- unmatch.stems(data.survival.carbon = survival.carbon)