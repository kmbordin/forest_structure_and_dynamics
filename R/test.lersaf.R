############################################################
############## EXAMPLE OF CODES' USE ######################
############################################################
### Code updated by Kauane M Bordin at 02 Oct 2025

### Example of how to use the codes available in this project
# please respect the order of each code!

# load packages ------
library(tidyverse)
library(BIOMASS)
library(vegan)
library(FD)
library(hillR)
library(here)
library(readxl)

# load functions ------
source("R/stem2abund.R")       # code 0, not a requirement!
source("R/correct.zombie.R")   # code 1
source("R/correct.diameter.R") # code 2
source("R/carbon.est.R")       # code 3
source("R/demography.R")       # code 4
source("R/unmatch.stems.R")    # code 5, not required, but recommended 
source("R/forest.dynamics.R")  # code 6
source("R/all.diversity.R")    # code 7


plot.info <- read_excel(here::here("data","plot_info_semMFO-02.xlsx"), sheet = 2) %>% 
  rename(Plot.Code = `Plot Code`)

WD <- read.csv2(here::here("data","wd_total.csv")) %>%
  rename(species = Species,
         WD = Iffsc) %>% 
  mutate(WD = as.numeric(WD)) %>% 
  dplyr::select(species, WD) %>% 
  add_row(species = "Campomanesia rhombea", WD= 0.68) %>% # genus
  add_row(species = "Eugenia coaetanea", WD= 0.66) %>% # genus
  add_row(species = "Eugenia indet", WD= 0.66) %>% # genus
  add_row(species = "Heisteria silviani", WD= 0.54) %>%   # all species mean
  add_row(species = "Indet indet", WD= 0.54) %>%  # all species mean
  add_row(species = "Laplacea acutifolia", WD= 0.57) %>%  # genus
  add_row(species = "Myrcia indet", WD= 0.59) %>% # genus
  add_row(species = "Ocotea indet", WD= 0.49) %>%  # genus
  add_row(species = "Prunus subcoriacea", WD= 0.60) %>% # genus
  add_row(species = "Xylosma pseudosalzmannii", WD= 0.65)  # genus
  

data <- read.csv(here::here("data","only_adult_data_2lastcensus.csv"))

data <- data %>% 
  left_join(plot.info, by="Plot.Code")

data.complete <- data %>% 
  rename(plotcode = Plot.Code,
         plot.area = `Ground Area (ha)`,
         census.yr = Census.Date,
         census.n = Census.No,
         treeid = TreeID,
         stem.gr.id = Stem.Group.ID,
         species = Species,
         d = D4,
         genus = Genus,
         family = Family,
         latitude = `Latitude Decimal`,
         longitude = `Longitude Decimal`) %>% 
  mutate(d = d/10) %>% # to cm
  dplyr::select(plotcode,plot.area,census.yr,census.n,treeid,stem.gr.id,
                d,genus,species,family,latitude,longitude,Height) %>% 
  mutate(plot.area = ifelse(plot.area==0.34, yes=0.12,no=plot.area)) %>% 
  mutate(Height = ifelse(Height==0, yes=mean(Height, rm=T),no=Height)) %>% 
  mutate(Height = ifelse(Height==0, yes=mean(Height, rm=T),no=Height)) %>% 
  filter(plotcode != "PRM-04") %>% 
  filter(plotcode != "PRM-05") %>% 
  left_join(WD, by="species")



# testing all functions ------
data.correct.for.zombie <- correct.zombie (data = data.complete)
data.correct.dbh.10cm.census1_2 <- correct.diameter (data = data.correct.for.zombie, 
                                                     census.numb = "1_2", dbh = 10)
# data.correct.dbh.10cm.census2_3 <- correct.diameter (data = data.correct.for.zombie, 
#                                                      census.numb = "2_3", dbh = 10)
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

trait <- data.complete %>% 
  ungroup() %>% 
  dplyr::select(species,WD) %>% 
  unique() %>% 
  remove_rownames() %>% column_to_rownames(var = "species")

all.diversity.metrics <- all.diversity (survival = survival.vital,
                                        recruitment = recruitment.vital,
                                        mortality = mortality.vital,
                                        trait = trait)
all.diversity.metrics$taxonomic.diversity
all.diversity.metrics$community_matrix_c1
all.diversity.metrics$community_matrix_c2
all.diversity.metrics$functional_census1
all.diversity.metrics$functional_census2
