##############################################################
##############  DATASET FOR TESTING  #########################
##############################################################
### Code updated by Kauane M Bordin at 10 Nov 2025

# MANDATORY: please check the species' names in both trait and demographic tables; they MUST match.
# variables   description                     format
# plotcode    unique plot name                factor 
# plot.area   plot area in hectares           numeric    
# census.n    census number                   numeric
# census.yr   census year                     numeric                  
# treeid      unique stem number              numeric
# stem.gr.id  stem to individualcommon code   character
# species     species name without authorship character   
# d           diameter in cm                  numeric
# genus       genus without authorship        character            
# family      species family                  character
# latitude    plot latitude                   numeric
# longitude   plot longitude                  numeric


data.complete <- data.frame(
  plotcode = c(rep("A", 9), rep("B", 13),rep("C",3)),
  plot.area = c(rep(1, 9), rep(0.2, 13),0.3,0.3,0.3),
  census.yr = c(2000,2000,2010,2010,2024,2024,2010,2000,2024,2005, 2005,2005,2020,2005,2020, 2005,2005,2020,2005,2005,2020,2020,2000,2000,2010),
  treeid = c(1,2,1,2,1,2,3,8,8,4,9,5,4,6,7, 80,81,81,90,91,92,93,333,334,333),
  stem.gr.id = c(rep(NA,15),"8a","8a","8a","9a","9a","9a","9a",NA,NA,NA),
  species = c("Araucaria angustifolia","Myrcia retorta","Araucaria angustifolia","Myrcia retorta", "Araucaria angustifolia", "Myrcia retorta", "Apuleia leiocarpa", "Apuleia leiocarpa","Apuleia leiocarpa", "Alchornea triplinervia","Casearia sylvestris", "Eugenia uniflora","Alchornea triplinervia", "Casearia sylvestris","Alsophila setosa",rep("Myrcia retorta",7), "Myrceugenia myrcioides","Araucaria angustifolia","Myrceugenia myrcioides"),
  d = c(19,25,11,100,19,100,10,10,16,50,21,16,55,22,10,10,10,11,10,10,12,12,20,10,20),
  genus = c("Araucaria","Myrcia","Araucaria","Myrcia","Araucaria","Myrcia", "Apuleia","Apuleia","Apuleia","Alchornea","Casearia","Eugenia","Alchornea", "Casearia","Alsophila", rep("Myrcia",7),"Myrceugenia","Araucaria","Myrceugenia"),
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
         WD = c(0.47,0.81,0.47,0.81,0.47,0.81,0.87,0.87,0.87,0.40, 0.62,0.72,0.40,0.62,0,0.81,0.81,0.81,0.81,0.81,0.81,0.81,0.62,0.47,0.62))

write.csv(data.complete,"raw-data/data.complete.H&WD.csv")

data.complete.H <- data.complete %>% select(-Height)
write.csv(data.complete.H,"raw-data/data.complete.WD.csv")

data.complete.WD <- data.complete %>% select(-WD)
write.csv(data.complete.WD,"raw-data/data.complete.H.csv")

data.complete.H.WD <- data.complete %>% select(-Height,WD)
write.csv(data.complete.H.WD,"raw-data/data.complete.csv")

trait <- data.complete %>% 
  select(species,WD) %>% unique() %>% 
  remove_rownames() %>% column_to_rownames(var="species")
write.csv(trait,"raw-data/trait.csv")
