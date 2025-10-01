############################################################
############## STEM TO ABUNDANCE-LEVEL #####################
############################################################
### Code developed by Kauane M Bordin at 01 Oct 2025

### Description: ForestPlots network data generates data in stem-level, 
# so we can find multi-stemmed trees instead of individuals

### In this code wefix this issue by creating a new data frame grouping 
# all multi-stemmed trees to only one individual (with the largest diameter).
# The information of number of stems is also retained.

# load packages ---------
library(tidyverse)

stem2abund <- function (data, census.number){ # dataset and number of census to summarise info
  
  #' @description generate abundance-level information
  #' @author Kauane Maiara Bordin
  #' @param  data frame with stem-level data 
  #' @return data frame with abundance-level data
  #' census.number: 1 == applies for all available censuses; 2, 3 or 4: filters census 2 to 4.
  #' data: stem-level information from ForestPlots.net format
  
  x <- data %>% filter (Census.No == census.number) # filters the desired census
  
  if(census.number == '2'){
    x = x %>% filter(F1 != "0") # F1 == 0 means dead stem - here we remove dead stems
  }
  else{
    x = x # if census number is 1, runs the code for all censuses
  }
  if(census.number == '3'){
    x = x %>% filter(F1 != "0")
  }
  else{
    x = x 
  }
  if(census.number == '4'){
    x = x %>% filter(F1 != "0") 
  }
  else{
    x = x 
  }
  
  x2 <- x %>% 
    mutate(Stem.Group.ID = ifelse(is.na(x$Stem.Group.ID), yes=x$TreeID, no= x$Stem.Group.ID)) %>%
    group_by(Stem.Group.ID,Census.No) %>% 
    add_count(Stem.Group.ID, name = "n.of.stems.per.ind") %>% # tracks the number of stems per individual
    ungroup() %>% 
    group_by(Stem.Group.ID,Census.No) %>% 
    slice_max(order_by = D4, n = 1) # largest dbh set as default individual size
  
  return(x2)
}

# usage: 
data.abund.all.censuses <- stem2abund(data = data.stem, census.number = 1)
data.abund.census2 <- stem2abund(data = data.stem, census.number = 2)
data.abund.census3 <- stem2abund(data = data.stem, census.number = 3)
data.abund.census4 <- stem2abund(data = data.stem, census.number = 4)