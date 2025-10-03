############################################################
##############  DIVERSITY METRICS  #########################
############################################################
### Code updated by Kauane M Bordin at 03 Oct 2025

### Description: taxonomic and functional diversity and composition

### In this code we can calculate taxonomic diversity (rarefied richness, 
# q-order (q0 = richness, q1 = Shannon index, q2 = Simpson index)), 
# functional diversity (RaoQ, FRic, FEve, FDiv), 
# taxonomic composition (NMDS), and functional composition (CWM).

# MANDATORY: please check the species' names in both trait and demographic tables; they MUST match.

# load packages ---------
library(tidyverse)
library(vegan)
library(FD)
library(hillR)

all.diversity <- function (survival,mortality,recruitment,trait){
  
  #' @description generate data for demographic rates and forest dynamics
  #' @author Kauane Maiara Bordin
  #' @param  data frame with individual (vital) level data across censuses
  #' @return data frame with taxonomic and functional diversity and composition 
  #'          based on individual density
  #' survival: vital metric, derived from demography function
  #' mortality: vital metric, derived from demography function
  #' recruitment: vital metric, derived from demography function
  #' trait: IF you want to obtain functional metrics, please provide a data frame 
  #'          with species, traits and trait values. If you do not want to calculate
  #'          functional metrics, please provide a table with species and WD values only. For this data frame, species should be rownames.
  #'          This code will not run without trait information. To be updated in the future. 
  
  
  # preparing data
  mort <- mortality
  rec <- recruitment
  surv <- survival
  survc1 <- surv %>% filter(census.n == 1)
  survc2 <- surv %>% filter(census.n == 2)
  
  # this is to sum species stems to obtain the total individual number
  census1 <- bind_rows(survc1,mort) %>% 
  dplyr::mutate(sp.n = 1) %>% # attribute a number for summing the individuals
  filter(species != "Indet indet")
  
  census2 <- bind_rows(survc2,rec)%>% 
  dplyr::mutate(sp.n = 1) %>% 
  filter(species != "Indet indet")
  
  print("Running taxonomic metrics")
  
  #community matrix from census 1 (species per plot described by the species abundance 
  comm.c1.density <- census1 %>% 
    ungroup() %>% 
    dplyr::select(plotcode,species,sp.n) %>% 
    tidyr::pivot_wider(names_from = species, values_from = c(sp.n), values_fn = sum) %>% 
    mutate(across(where(is.numeric), tidyr::replace_na, 0)) %>% # if there is NA, replace to 0
    arrange(plotcode) %>% 
    remove_rownames() %>% column_to_rownames(var = "plotcode")
  
  #community matrix from census 2 (species per plot described by the species abundance 
  comm.c2.density <- census2 %>% 
    ungroup() %>% 
    dplyr::select(plotcode,species,sp.n) %>% 
    tidyr::pivot_wider(names_from = species, values_from = c(sp.n), values_fn = sum) %>% 
    mutate(across(where(is.numeric), tidyr::replace_na, 0)) %>% # if there is NA, replace to 0
    arrange(plotcode)%>% 
    remove_rownames() %>% column_to_rownames(var = "plotcode")
  
  # calculate rarefied richness for census 1
  S.census1 <- specnumber(comm.c1.density) # observed number of species
  raremax.census1 <- min((S.census1))
  Srare.census1 <- rarefy(comm.c1.density, raremax.census1)

  # calculate rarefied richness for census 2
  S.census2 <- specnumber(comm.c2.density) # observed number of species
  raremax.census2 <- min((S.census2))
  Srare.census2 <- rarefy(comm.c2.density, raremax.census2)

  # taxonomic diversity calculated through hill numbers (Chao et al 2014)
  #where q0=richness, q1=shannon index, and q=2 inv.simpson index;rarefied richness is also shown
  diversity.density <- data.frame(rich.dens.census1 = hill_taxa(comm = comm.c1.density, q = 0),
                                shannon.dens.census1 = hill_taxa(comm = comm.c1.density, q = 1),
                                inv.simpson.dens.census1 = hill_taxa(comm = comm.c1.density, q = 2),
                                rich.dens.census2 = hill_taxa(comm = comm.c2.density, q = 0),
                                shannon.dens.census2 = hill_taxa(comm = comm.c2.density, q = 1),
                                inv.simpson.dens.census2 = hill_taxa(comm = comm.c2.density, q = 2),
                                rarefied.richness.c1 = Srare.census1,
                                rarefied.richness.c2 = Srare.census2,
                                plotcode = rownames(comm.c1.density))
  
  # NMDS ordination
  nmds_census1 <- metaMDS(comm.c1.density, distance = "bray", autotransform = FALSE)
  nmds_census1
  nmds.census1.scores <- as.data.frame(nmds_census1$points)
  
  png('results/NMDS.census1.png', units="in", width=5, height=5, res=300)
  ggplot(nmds.census1.scores, mapping = aes(x = MDS1, y = MDS2)) + geom_point()+
    geom_vline(xintercept=0, color="black", linetype="dotted") +
    geom_hline(yintercept=0, color="black", linetype="dotted") +
    theme_light()+
    theme(panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        legend.position = "bottom",
                        panel.background = element_blank())
    dev.off()
  
  nmds_census2 <- metaMDS(comm.c2.density, distance = "bray", autotransform = FALSE)
  nmds_census2
  nmds.census2.scores <- as.data.frame(nmds_census1$points)
  
  png('results/NMDS.census2.png', units="in", width=5, height=5, res=300)
  ggplot(nmds.census2.scores, mapping = aes(x = MDS1, y = MDS2)) + geom_point()+
    geom_vline(xintercept=0, color="black", linetype="dotted") +
    geom_hline(yintercept=0, color="black", linetype="dotted") +
    theme_light()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          panel.background = element_blank())
    dev.off()
    
    print("Running functional metrics")
    # functional diversity and composition
  comm.c1.density
  comm.c2.density
  trait
  
  trait1 <- trait %>% 
    mutate(sp = rownames(trait)) %>% 
    filter(sp %in% colnames(comm.c1.density)) %>% 
    arrange(sp) %>%  # order rownames
    remove_rownames() %>% column_to_rownames(var = 'sp')
    
  trait2 <- trait %>% 
    mutate(sp = rownames(trait)) %>% 
    filter(sp %in% colnames(comm.c2.density)) %>% 
    arrange(sp) %>%  # order rownames
    remove_rownames() %>% column_to_rownames(var = 'sp')
  
  # order colnames
  comm.c1.density <- comm.c1.density %>% 
    dplyr::select(order(colnames(comm.c1.density)))
  comm.c2.density <- comm.c2.density %>% 
    dplyr::select(order(colnames(comm.c2.density)))
  
  print("checking species match for community and trait data frame")
  print("community at census 1")
  print(setdiff(colnames(comm.c1.density), rownames(trait1)))
  print(setdiff(rownames(trait1),colnames(comm.c1.density)))
  
  print("community at census 2")
  print(setdiff(colnames(comm.c2.density), rownames(trait2)))
  print(setdiff(rownames(trait2),colnames(comm.c2.density)))
  
  print("If there is a mismatch, the code will stop")
  
  comm1 <- as.matrix(comm.c1.density)
  comm2 <- as.matrix(comm.c2.density)
  

  functionalcensus1 <- dbFD(x = trait1, a = comm1)
  print("functional metrics from census 1 successfully obtained")
  functionalcensus2 <- dbFD(x = trait2, a = comm2)
  print("functional metrics from census 2 successfully obtained")
  
  estimates <- list(taxonomic.diversity = diversity.density, 
                    functional_census1 = functionalcensus1,
                    functional_census2 = functionalcensus2,
                    community_matrix_c1 = comm.c1.density,
                    community_matrix_c2 = comm.c2.density) 
  
  return(estimates)
}
# usage: ------------------------------------------------------------------ 
# all.diversity.metrics <- all.diversity (survival = survival.vital,
#                                         recruitment = recruitment.vital, 
#                                         mortality = mortality.vital,
#                                         trait = trait)
