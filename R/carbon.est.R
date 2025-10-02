############################################################
##############  CARBON ESTIMATES  ##########################
############################################################
### Code updated by Kauane M Bordin at 02 Oct 2025

### Description: biomass (AGB) and carbon (AGC) estimates using data for different sources

### In this code we can obtain AGB and AGC using refined data of WD and individual height
# but also WD from global databases (Zanne et al 2014) and E - environmental stress.
# the parameters allow to use both sources as well - local WD and E.
# It is strongly advised to run 'correct.diameter' function before to correct for any 
# inconsistencies in dbh measurement.

# load packages ---------
library(tidyverse)
library(BIOMASS)

### Required format for column names:
# use plotcode instead of Plot.Code
# use treeid instead of TreeID
# use plot.area for the area of plot
# use stem.gr.id instead of Stem.Group.ID
# use species instead of Species
# use genus instead of Genus
# use family instead of Family
# use latitude instead of Latitude.Decimal
# use longitude instead of Longitude.Decimal
# use census.n instead of Census.No
# use d instead of D4
# use d in cm instead of d in mm
# census.yr instead of Census.Date
# census.yr in format as.numeric
# use Height in meters and format as.numeric

carbon.est <- function (data, WD.info, H.info, dbh){
  
  #' @description generate stem-level biomass and carbon estimates
  #' @author Kauane Maiara Bordin
  #' @param  data frame with stem level data 
  #' @return data frame with stem level biomass and carbon 
  #' data: stem-level information from ForestPlots.net format, but be careful with 
  #'        the required format for column names. I strongly advise to run 
  #'        'correct.diameter' function before running this function.
  #' WD.info: TRUE or FALSE; If WD.info == TRUE, the species and WD values must be provided;
  #'          If WD.info == FALSE, WD will be obtained from the global database (Zanne et al 2014)
  #'  H.info: TRUE or FALSE; If H.info == TRUE, a vector of individual height must be provided;
  #'          If H.info == FALSE, the code will use the E (environmental stress) parameter from Chave et al (2014) and a vector of stem latitude and longitude must be provided.      
  #' dbh: minimum dbh estimate carbon - please check your data! here dbh is in cm 
  
  ferns.families <- c("Cyatheaceae", "Dicksoniaceae","Pteridaceae") #fern families
  data <- data %>% 
    filter(d >= dbh) %>%  #to ensure same dbh threshold for all stems/dbh must be in cm
    filter(! family %in% ferns.families) #remove fern families
  
  if(WD.info==TRUE){
    WD <- data %>% 
      dplyr::select(species,WD)
    }
  
  if(WD.info==FALSE){  #get wood density values for species from Zanne et al 2014
    WD <- BIOMASS::getWoodDensity(data$genus, data$species) %>% 
      dplyr::select(species,meanWD,levelWD) %>% 
      rename(WD = meanWD) %>% 
      unique()
    }
  
  if(H.info==TRUE){
    H <- data %>% 
      dplyr::select(Height) 
  }
  
  if(H.info==FALSE){  #get wood density values for species from Zanne et al 2014
    H <- data %>% 
      dplyr::select(longitude,latitude)
  }

  ### estimate AGB and AGC
  # data
  # WD
  # H
  
  if(H.info==TRUE & WD.info==TRUE){
    biomass  <-  BIOMASS::computeAGB(D = data$d, WD = WD$WD, H = H$Height)
    biomass <- data.frame(AGB = biomass) %>% 
      dplyr::mutate(AGC = AGB*0.456)  #estimate AGC based on Martin et al 2018
  }
  
  if(H.info==FALSE & WD.info==TRUE){
    WD <- unique(WD)
    data.c <- left_join(data,WD, by="species") %>% 
      mutate(WD = WD.x)
    biomass  <-  BIOMASS::computeAGB(D = data.c$d, WD = data.c$WD, coord = H)
    biomass <- data.frame(AGB = biomass) %>% 
      dplyr::mutate(AGC = AGB*0.456)  #estimate AGC based on Martin et al 2018
  }
  
  if(H.info==FALSE & WD.info==FALSE){
    WD <- unique(WD)
    data.c <- left_join(data,WD, by="species") 
    biomass  <-  BIOMASS::computeAGB(D = data.c$d, WD = data.c$WD, coord = H)
    biomass <- data.frame(AGB = biomass) %>% 
      dplyr::mutate(AGC = AGB*0.456)  #estimate AGC based on Martin et al 2018
  }
  
  if(H.info==TRUE & WD.info==FALSE){
    WD <- unique(WD)
    data.c <- left_join(data,WD, by="species") 
    biomass  <-  BIOMASS::computeAGB(D = data.c$d, WD = data.c$WD, H = H$Height)
    biomass <- data.frame(AGB = biomass) %>% 
      dplyr::mutate(AGC = AGB*0.456)  #estimate AGC based on Martin et al 2018
  }
  
  data.complete <- bind_cols(data,biomass)
  print(head(data.complete))
  return(data.complete)
}

# usage: ------------------------------------------------------------------ 
# WD.info==TRUE and H.info==TRUE
# carb = carbon.est(data = data, WD.info = TRUE, H.info = TRUE, dbh = 10)

# WD.info==FALSE and H.info==FALSE
# data2 = data %>% dplyr::select(-c(WD,Height))
# carb2 = carbon.est(data = data2 ,WD.info = FALSE, H.info = FALSE, dbh = 10)

# WD.info==TRUE and H.info==FALSE
# data3 = data %>% dplyr::select(-c(Height))
# carb3 = carbon.est(data = data3, WD.info = TRUE, H.info = FALSE,dbh = 10)

# WD.info==FALSE and H.info==TRUE
# data4 = data %>% dplyr::select(-c(WD))
# carb4 = carbon.est(data = data4, WD.info = FALSE, H.info = TRUE,dbh = 10)
