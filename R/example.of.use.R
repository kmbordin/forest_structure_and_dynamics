############################################################
############## EXAMPLE OF CODES' USE ######################
############################################################
### Code updated by Kauane M Bordin at 02 Oct 2025

### Example of how to use the codes available in this project
# please respect the order of each code!

# load packages ------
library(tidyverse)

# load functions ------
source("R/stem2abund.R")       # code 0, not a requirement!
source("R/correct.zombie.R")   # code 1
source("R/correct.diameter.R") # code 2
source("R/carbon.est.R")       # code 3
source("R/demography.R")       # code 4
source("R/unmatch.stems.R")    # code 5, not required, but recommended 
source("R/forest.dynamics.R")  # code 6


data <- data.frame(plotcode = c(rep("A", 9), rep("B", 13)),
                                      plot.area = c(rep(1, 9), rep(0.2, 13)),
                                      census.yr = c(2000,2000,2010,2010,2024,2024,2010,2000,2024,2005,2005,2005,2020,2005,2020,
                                                                                    2005,2005,2020,2005,2005,2020,2020),
                                      treeid = c(1,2,1,2,1,2,3,8,8,4,9,5,4,6,7, 80,81,81,90,91,92,93),
                                    stem.gr.id = c(rep(NA,15),"8a","8a","8a","9a","9a","9a","9a"),
                                     species = c("Araucaria angustifolia","Myrcia retorta","Araucaria angustifolia","Myrcia retorta","Araucaria angustifolia","Myrcia retorta","Apuleia leiocarpa","Apuleia leiocarpa","Apuleia leiocarpa","Alchornea triplinervia","Casearia sylvestris","Eugenia uniflora","Alchornea triplinervia","Casearia sylvestris","Alsophila setosa",rep("Myrcia retorta",7)),
                                 d = c(19,25,11,100,19,100,10,10,16,50,21,16,55,22,10,10,10,11,10,10,12,12),
                                    genus = c("Araucaria","Myrcia","Araucaria","Myrcia","Araucaria","Myrcia","Apuleia","Apuleia","Apuleia","Alchornea","Casearia","Eugenia","Alchornea","Casearia","Alsophila", rep("Myrcia",7)),
                                  family = c("Araucariaceae","Myrtaceae","Araucariaceae","Myrtaceae","Araucariaceae","Myrtaceae","Fabaceae","Fabaceae","Fabaceae","Euphorbiaceae","Salicaceae","Myrtaceae","Euphorbiaceae","Salicaceae","Cyatheaceae",rep("Myrtaceae",7)),
                                    latitude = c(rep(-27.84, 9), rep(-29.61, 13)),
                                    longitude = c(rep(-50.23, 9), rep(-50.18, 13))) %>% 
      mutate(census.n = ifelse(census.yr %in% c(2000,2005),1,NA),
                         census.n = ifelse(census.yr %in% c(2010,2020),2,census.n),
                          census.n = ifelse(census.yr %in% c(2024),3,census.n),
                          Height = c(12,10,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22),
                          WD = c(0.47,0.81,0.47,0.81,0.47,0.81,0.87,0.87,0.87,0.40,0.62,0.72,0.40,0.62,0,0.81,0.81,0.81,0.81,0.81,0.81,0.81))

