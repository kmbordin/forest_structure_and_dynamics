############################################################
##############  CORRECT DIAMETER  ##########################
############################################################
### Code developed by Kauane M Bordin at 01 Oct 2025

### Description: Sometimes long-term monitoring fails in tracking stem diameter
# and we observe abnormal absolute growth rates (positive or negative).

### In this code we fix this issue by setting a limit of maximum growth rates (positive =  4 cm/yr, negative = -0.5cm/yr) for each stem. 
# If the stems show abnormal growth, the corrections applied here assume 0 growth rates to avoid 
# any kind of bias. If the stem shows positive abnormal growth, we set dbh in census 2 as default;
# If the stem shows negative abnormal growth, we set dbh in census 1 as default.

# load packages ---------
library(tidyverse)

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

correct.diameter <- function (data,census.n,dbh){
  
  #' @description correct diameter to avoid abnormal growth rates
  #' @author Kauane Maiara Bordin (kauanembordin[at]gmail.com)
  #' @param  data frame with stem-level data 
  #' @return data frame with diameter corrected for abnormal growth
  #' census.number: 1 == applies for all available censuses; 2, 3 or 4: filters census 2 to 4.
  #' data: stem-level information from ForestPlots.net format, but be careful with the required format for column names
  #' census.n: "1_2", "2_3" ... "6_7"
  #' dbh = minimum dbh to apply the correction - please check your data! here dbh is in cm 
   
  ferns.families <- c("Cyatheaceae", "Dicksoniaceae","Pteridaceae") #fern families to be removed
  
  data <- data %>% 
    filter(d >= d) %>%  # to ensure same dbh threshold for all stems. dbh must be in cm!
    filter(! family %in% ferns.families) # remove ferns families
  
  original.census.n = census.n # save original info
  
  if(census.n=="1_2") { # data filtering for census 1 and 2
    census1 = data %>% filter(census.n == 1)
    census2 = data %>% filter(census.n == 2)
    data = bind_rows(census1,census2)
  }
  
  if(census.n=="2_3"){ # data filtering for census 2 and 3
    #census1
    census1 = data %>% filter(census.n == 2) %>% 
      mutate(census.n = replace(census.n, census.n == "2", "1")) 
    #census2
    census2 = data %>% filter(census.n == 3) %>% 
      mutate(census.n = replace(census.n, census.n == "3", "2")) 
    c2 = unique(census2$plotcode)
    census1 <- census1 %>% 
      filter(plotcode %in% c2)
    data = bind_rows(census1,census2)
  }
  if(census.n=="3_4"){ # data filtering for census 3 and 4
    #census1
    census1 = data %>% filter(census.n == 3) %>% 
      mutate(census.n = replace(census.n, census.n == "3", "1")) 
    #census2
    census2 = data %>% filter(census.n == 4) %>% 
      mutate(census.n = replace(census.n, census.n == "4", "2")) 
    c2 = unique(census2$plotcode)
    census1 <- census1 %>% 
      filter(plotcode %in% c2)
    data = bind_rows(census1,census2)
  }
  if(census.n=="4_5"){ # data filtering for census 4 and 5
    #census1
    census1 = data %>% filter(census.n == 4) %>% 
      mutate(census.n = replace(census.n, census.n == "4", "1")) 
    #census2
    census2 = data %>% filter(census.n == 5) %>% 
      mutate(census.n = replace(census.n, census.n == "5", "2")) 
    c2 = unique(census2$plotcode)
    census1 <- census1 %>% 
      filter(plotcode %in% c2)
    data = bind_rows(census1,census2)
    #census2
    census2 = data %>% filter(census.n == 6) %>% 
      mutate(census.n = replace(census.n, census.n == "6", "2")) 
    c2 = unique(census2$plotcode)
    census1 <- census1 %>% 
      filter(plotcode %in% c2)
    data=bind_rows(census1,census2)
  }
  if(census.n=="6_7"){ # data filtering for census 6 and 7
    #census1
    census1 = data %>% filter(census.n == 6) %>% 
      mutate(census.n = replace(census.n, census.n == "6", "1")) 
    #census2
    census2 = data %>% filter(census.n == 7) %>% 
      mutate(census.n = replace(census.n, census.n == "7", "2")) 
    c2 = unique(census2$plotcode)
    census1 <- census1 %>% 
      filter(plotcode %in% c2)
    data = bind_rows(census1,census2)
  }
  
  # The code below aims to summarise the census, species, and plot information
  
  interval <- data %>%
    dplyr::select(plotcode,census.n, census.yr) %>% 
    group_by(plotcode, census.n) %>%
    mutate(original.census = original.census.n,
           row_id = row_number()) %>%  # identify duplicate values
    pivot_wider(names_from = census.n, # pivot table to allow summarisation of plots and censuses
                values_from = census.yr,
                names_prefix = "census",
                values_fn = list(census.yr = list)) %>%  
    unnest(cols = everything()) %>%  
    select(-row_id) %>%  # remove id of duplicates
    unique() %>% 
    drop_na() %>% 
    mutate(census.interv = census2-census1) %>% # calculate census interval in years
    dplyr::select(-c(census2,census1))
  
  # summarise plot information
  data.summarised <- data %>% 
    dplyr::select(plotcode,plot.area,latitude,longitude) %>% 
    unique()
  
  # summarise census information
  data.summarised2 <- data %>% 
    dplyr::select(plotcode,census.yr,census.n) %>% 
    unique() %>% 
    group_by(plotcode) %>% 
    pivot_wider(id_cols = NULL,names_from = census.n ,values_from = census.yr) %>% 
    rename(census.number1 = `1`,
           census.number2 = `2`)
  
  #summarise species information
  data.summarised3 <- data %>% 
    dplyr::select(species,family,genus) %>% 
    unique()
  
  # the following code effectively corrects for the dbh between census 
  # the corrections are applied to dbhs that present abnormal growth (negative or positive).
  # abnormal negative growth relates to growth rates lower than -0.5 cm/yr
  # abnormal positive growth is higher than 4 cm/yr.
  
  correct.dbh <- data %>% 
    dplyr:: select(plotcode, species, census.n, treeid, d, stem.gr.id,census.yr,census.n) %>%
    left_join(interval, by = "plotcode") %>% # unite census info
    group_by(plotcode, species, census.n, treeid, d, stem.gr.id, census.interv,original.census) %>% # large grouping to avoid any mistakes on tree growth correction
    summarise(d = mean(d, na.rm = TRUE), .groups = "drop") %>% 
    pivot_wider(names_from = c(census.n),
                values_from = d, 
                names_prefix = "census") %>% 
    relocate(census1, .before = census2) %>% 
    mutate(gr =(census2-census1)/census.interv) %>% # calculate absolute growth rates per year in cm
    mutate(gr2 = ifelse(gr < (-0.5), (-0.5), gr), # test for abnormal negative growth -- if the absolute growth is lower than -0.5, set the value as -0.5 to be filtered next
           gr.corr = ifelse(gr2 < 4, gr2, 4), # test for abnormal positive growth -- if the growth is larger than 4, set the value as 4 to be filtered next
           d1.cor = ifelse(gr.corr == (4), census2, census1), # if census 2 has larger dbh than census 1, we determined dbh2 == dbh1 by setting dbh2 as default, thus dbh1 will be corrected to dbh1 == dbh2
           d2.cor = ifelse(gr.corr == (-0.5), census1, census2),# if census 1 has larger dbh than census 2, we determined dbh1 == dbh2 by setting dbh1 as default, thus dbh2 will be corrected to dbh2 == dbh1
           d1.cor = ifelse(is.na(d1.cor), census1, d1.cor), # complete empty cells
           d2.cor = ifelse(is.na(d2.cor), census2, d2.cor), # complete empty cells
           d1.cor = ifelse(is.na(d1.cor), NA, d1.cor), # maintain NAs to be dropped later
           d2.cor = ifelse(is.na(d2.cor), NA, d2.cor)) %>% # maintain NAs to be dropped later
    dplyr::select(plotcode,species,treeid,stem.gr.id,d1.cor,d2.cor,original.census) %>% 
    left_join(data.summarised, by = "plotcode") %>% # unite plot info
    pivot_longer(cols = c(d1.cor,d2.cor), names_to = "census", values_to = "d") %>% 
    separate(original.census, c("original.time1","original.time2")) %>% 
    mutate(census.n = NA,
           census.n = ifelse(census == "d1.cor", original.time1, census.n), # include census number
           census.n = ifelse(census == "d2.cor", original.time2, census.n)) %>% 
    ungroup() %>% 
    left_join(data.summarised2, by = c("plotcode")) %>% 
    mutate(census.yr = NA,
           census.yr = ifelse(census.n == min(census.n), census.number1,census.yr),
           census.yr = ifelse(census.n == max(census.n),census.number2, census.yr)) %>% 
    dplyr::select(-c(census.number1,census.number2,original.time1,original.time2,census)) %>% 
    drop_na(d) %>% # drop the NA values in dbh
    left_join(data.summarised3, by = "species") %>% # unite species info
    dplyr::select(plotcode,plot.area,census.n,census.yr,treeid,stem.gr.id,species,d,genus,family,latitude,longitude) %>% 
    mutate(stem.gr.id = as.character(stem.gr.id))
  
  data <- correct.dbh # set the dataframe of corrected dbh as the default
  return(data)
}

# usage: ------------------------------------------------------------------ 
# data.correct.dbh.10cm.census1_2 <- correct.diameter (data = data, census.n = "1_2", dbh = 10)
# data.correct.dbh.5cm.census2_3 <- correct.diameter (data = data, census.n = "2_3", dbh = 5)

