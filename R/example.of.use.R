############################################################
############## EXAMPLE OF CODES' USE ######################
############################################################
### Code updated by Kauane M Bordin at 04 Oct 2025

### Example of how to use the codes available in this project
# please respect the order of each code!

# load packages ------
library(tidyverse)
library(BIOMASS)
library(vegan)
library(FD)
library(hillR)

# load functions ------
source("R/correct.zombie.R")   # code 1
source("R/correct.diameter.R") # code 2
source("R/carbon.est.R")       # code 3
source("R/demography.R")       # code 4
source("R/unmatch.stems.R")    # code 5, not required, but recommended 
source("R/forest.dynamics.R")  # code 6
source("R/all.diversity.R")    # code 7


data.complete <- data.frame(plotcode = c(rep("A", 9), rep("B", 13),rep("C",3)),
                            plot.area = c(rep(1, 9), rep(0.2, 13),rep(0.3,3)),
                            census.yr = c(2000,2000,2010,2010,2024,2024,2010,2000,2024,2005,
                                          2005,2005,2020,2005,2020, 2005,2005,2020,2005,2005,
                                          2020,2020,2000,2000,2010),
                            treeid = c(1,2,1,2,1,2,3,8,8,4,9,5,4,6,7,80,81,81,90,91,92,93,333,334,333),
                            stem.gr.id = c(rep(NA,15),"8a","8a","8a","9a","9a","9a","9a",NA,NA,NA),
                            species = c("Araucaria angustifolia","Myrcia retorta","Araucaria angustifolia",
                                        "Myrcia retorta","Araucaria angustifolia","Myrcia retorta",
                                        "Apuleia leiocarpa","Apuleia leiocarpa","Apuleia leiocarpa",
                                        "Alchornea triplinervia","Casearia sylvestris",
                                        "Eugenia uniflora","Alchornea triplinervia",
                                        "Casearia sylvestris","Alsophila setosa",rep("Myrcia retorta",7), 
                                        "Eugenia indet","Araucaria angustifolia","Eeugenia indet"),
                            d = c(19,25,11,100,19,100,10,10,16,50,21,16,55,22,10,10,10,11,10,10,12,12,20,10,20),
                            genus = c("Araucaria","Myrcia","Araucaria","Myrcia","Araucaria","Myrcia",
                                      "Apuleia","Apuleia","Apuleia","Alchornea","Casearia","Eugenia","Alchornea",
                                      "Casearia","Alsophila", rep("Myrcia",7),"Eugenia","Araucaria","Eugenia"),
                            family = c("Araucariaceae","Myrtaceae","Araucariaceae","Myrtaceae",
                                       "Araucariaceae","Myrtaceae","Fabaceae","Fabaceae","Fabaceae",
                                       "Euphorbiaceae","Salicaceae","Myrtaceae","Euphorbiaceae",
                                       "Salicaceae","Cyatheaceae",rep("Myrtaceae",8),"Araucariaceae","Myrtaceae"),
                            latitude = c(rep(-27.84, 9), rep(-29.61, 16)),
                            longitude = c(rep(-50.23, 9), rep(-50.18, 16))) %>% 
  mutate(census.n = ifelse(census.yr %in% c(2000,2005),1,NA),
         census.n = ifelse(census.yr %in% c(2010,2020),2,census.n),
         census.n = ifelse(census.yr %in% c(2024),3,census.n),
         Height = c(12,10,3,4,5,6,7,8,9,10,11,5,13,7,15,16,17,10,19,20,3,22,12,10,12),
         WD = c(0.47,0.81,0.47,0.81,0.47,0.81,0.87,0.87,0.87,0.40,
                0.62,0.72,0.40,0.62,0,0.81,0.81,0.81,0.81,0.81,0.81,0.81,0.62,0.47,0.62))

trait <- data.complete %>% 
  ungroup() %>% 
  dplyr::select(species,WD) %>% 
  unique() %>% 
  remove_rownames() %>% column_to_rownames(var = "species")

shiny::runApp("R/shiny.R")

# testing all functions ------
data.correct.for.zombie <- correct.zombie (data = data.complete)
data.correct.dbh.10cm.census1_2 <- correct.diameter (data = data.correct.for.zombie, 
                                                     census.numb = "1_2", dbh = 10)
data.correct.dbh.10cm.census2_3 <- correct.diameter (data = data.correct.for.zombie, 
                                                     census.numb = "2_3", dbh = 10)
# WD.info==TRUE and H.info==TRUE
carb = carbon.est(data = data.correct.dbh.10cm.census1_2, WD.info = TRUE, H.info = TRUE, dbh = 10)

# WD.info==FALSE and H.info==FALSE
data2 = data.correct.dbh.10cm.census1_2 %>% dplyr::select(-c(WD,Height))
carb2 = carbon.est(data = data2 ,WD.info = FALSE, H.info = FALSE, dbh = 10)

# WD.info==TRUE and H.info==FALSE
data3 = data.correct.dbh.10cm.census1_2 %>% dplyr::select(-c(Height))
carb3 = carbon.est(data = data3, WD.info = TRUE, H.info = FALSE,dbh = 10)

# WD.info==FALSE and H.info==TRUE
data4 = data.correct.dbh.10cm.census1_2 %>% dplyr::select(-c(WD))
carb4 = carbon.est(data = data4, WD.info = FALSE, H.info = TRUE,dbh = 10)

data <- carb
survival.vital <- demography(data = data, metric = "vital", rate = "survival", census.n = "1_2")
recruitment.vital <- demography(data = data, metric = "vital", rate = "recruitment", census.n = "1_2")
mortality.vital <- demography(data = data, metric = "vital", rate = "mortality", census.n = "1_2")
survival.carbon <- demography(data = data, metric = "carbon", rate = "survival", census.n = "1_2")
recruitment.carbon <- demography(data = data, metric = "carbon", rate = "recruitment", census.n = "1_2")
mortality.carbon <- demography(data = data, metric = "carbon", rate = "mortality", census.n = "1_2")

survival.carbon <- unmatch.stems(data.survival.carbon = survival.carbon)
demographic.rates <- demography.and.dynamics(survival = survival.vital,
                                             recruitment = recruitment.vital,
                                             mortality = mortality.vital,
                                             metric = "vital")
forest.dynamics <- demography.and.dynamics(survival = survival.carbon,
                                           recruitment = recruitment.carbon,
                                           mortality = mortality.carbon,
                                           metric = "carbon")

all.diversity.metrics <- all.diversity (survival = survival.vital,
                                        recruitment = recruitment.vital,
                                        mortality = mortality.vital,
                                        trait = trait)
all.diversity.metrics$taxonomic.diversity
all.diversity.metrics$community_matrix_c1
all.diversity.metrics$community_matrix_c2
all.diversity.metrics$functional_census1
all.diversity.metrics$functional_census2



