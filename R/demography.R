############################################################
##############  DEMOGRAPHIC DATASETS  ######################
############################################################
### Code updated by Kauane M Bordin at 02 Oct 2025

### Description: separates the dataset into survivors, recruits, and deads, 
# for both vital rates (individual-level) and carbon dynamics (stem-level)

### In this code we can obtain information of survival, recruitment, and mortality 
# to calculate forest dynamics metrics. Requires the dataset derived from carbon.est function

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

demography <- function (data,metric,rate,census.n){
  
  #' @description generate data for demography
  #' @author Kauane Maiara Bordin
  #' @param data dataframe with stem level data across censuses, for metrics "vital" - vital rates, or "carbon" - carbon estimates
  #' @return dataframe with stem level info of survivors, recruits, or deads
  #' data: stem-level information from ForestPlots.net format, but be careful with 
  #'       the required format for column names. It is necessary to run the 
  #'       carbon.est function before running this function.
  #' metric: "vital" or "carbon"; vital for individual-level, and carbon for stem-level
  #' rate: "survival", "recruitment", or "mortality"
  #' census.n: "1_2", "2_3" ... "7_8"
  
  # filters to apply for different census intervals
  if(census.n=="1_2"){
    #census1
    census1 = data %>% filter(census.n==1)
    #census2
    census2 = data %>% filter(census.n==2) 
    data = bind_rows(census1,census2)
  }
  
  if(census.n=="2_3"){
    #census1
    census1 = data %>% filter(census.n==2) %>% 
      mutate(census.n = replace(census.n, census.n == "2", "1")) 
    #census2
    census2 = data %>% filter(census.n==3) %>% 
      mutate(census.n = replace(census.n, census.n == "3", "2")) 
    c2 = unique(census2$plotcode)
    census1 <- census1 %>% 
      filter(plotcode %in% c2)
    data = bind_rows(census1,census2)
  }
  
  if(census.n=="3_4"){
    #census1
    census1 = data %>% filter(census.n==3)%>% 
      mutate(census.n = replace(census.n, census.n == "3", "1")) 
    #census2
    census2 = data %>% filter(census.n==4) %>% 
      mutate(census.n = replace(census.n, census.n == "4", "2")) 
    c2 = unique(census2$plotcode)
    census1 <- census1 %>% 
      filter(plotcode %in% c2)
    data = bind_rows(census1,census2)
  }
  
  if(census.n=="4_5"){
    #census1
    census1 = data %>% filter(census.n==4)%>% 
      mutate(census.n = replace(census.n, census.n == "4", "1")) 
    #census2
    census2 = data %>% filter(census.n==5) %>% 
      mutate(census.n = replace(census.n, census.n == "5", "2")) 
    c2 = unique(census2$plotcode)
    census1 <- census1 %>% 
      filter(plotcode %in% c2)
    data = bind_rows(census1,census2)
  }
  
  if(census.n=="5_6"){
    #census1
    census1 = data %>% filter(census.n==5)%>% 
      mutate(census.n = replace(census.n, census.n == "5", "1")) 
    #census2
    census2 = data %>% filter(census.n==6) %>% 
      mutate(census.n = replace(census.n, census.n == "6", "2")) 
    c2 = unique(census2$plotcode)
    census1 <- census1 %>% 
      filter(plotcode %in% c2)
    data = bind_rows(census1,census2)
  }
  
  if(census.n=="6_7"){
    #census1
    census1 = data %>% filter(census.n==6)%>% 
      mutate(census.n = replace(census.n, census.n == "6", "1")) 
    #census2
    census2 = data %>% filter(census.n==7) %>% 
      mutate(census.n = replace(census.n, census.n == "7", "2")) 
    c2 = unique(census2$plotcode)
    census1 <- census1 %>% 
      filter(plotcode %in% c2)
    data = bind_rows(census1,census2)
  }
  
  if(census.n=="7_8"){
    #census1
    census1 = data %>% filter(census.n==7)%>% 
      mutate(census.n = replace(census.n, census.n == "7", "1")) 
    #census2
    census2 = data %>% filter(census.n==8) %>% 
      mutate(census.n = replace(census.n, census.n == "8", "2")) 
    c2 = unique(census2$plotcode)
    census1 <- census1 %>% 
      filter(plotcode %in% c2)
    data = bind_rows(census1,census2)
  }
  
  # summarise census 1 date
  date1 <- census1 %>% 
    group_by(plotcode) %>% 
    summarise(census.yr1 = mean(census.yr))
  
  # summarise census 2 date
  date2 <- census2 %>% 
    group_by(plotcode) %>% 
    summarise(census.yr2 = mean(census.yr))
  
  # combine dataset
  dates <-  inner_join(date1,date2)
  
  # filtering per metric -----
  
  if(metric == "vital"){
    #filter first census
    census1 <- data %>% 
      filter(census.n == 1)
    
    # multi-stemmed trees
    multi_census1 <- census1 %>% 
      filter(!is.na(stem.gr.id == TRUE)) %>% # filter all stems with stem group id
      group_by(stem.gr.id, plotcode, family, species) %>% 
      summarise(census.yr = mean(census.yr),
                n.stems = n(),# count the number of multi-stems per individual
                d = max(d)) %>% 
      rename(treeid = stem.gr.id) %>% 
      mutate(treeid = as.character(treeid))
    
    # single-stemmed trees
    one_census1 <- census1 %>% 
      filter(!is.na(stem.gr.id) == FALSE) %>% # filter all stem that do not have stem group id
      select(treeid, plotcode, family, species, census.yr, d) %>% 
      rename(census.yr = census.yr)%>% 
      mutate(treeid=as.character(treeid),
             n.stems = 1)
    
    total_census1 <- bind_rows(multi_census1, one_census1) %>% 
      mutate(status = 1, 
             census.n = 1)
    
    # filter second census
    census2 <- data %>% 
      filter(census.n == 2)
    
    # multi-stemmed trees
    multi_census2 <- census2 %>% 
      filter(!is.na(stem.gr.id == TRUE)) %>% # filter all stem with stem group id
      group_by(stem.gr.id, plotcode, family, species) %>% 
      summarise(census.yr = mean(census.yr),
                d = max(d),
                n.stems = n()) %>% # count multi-stemmed trees
      rename(treeid = stem.gr.id) %>% 
      mutate(treeid=as.character(treeid),
             n.stems = 1,
             status=1)
    
    # single-stemmed trees
    one_census2 <- census2 %>% 
      filter(!is.na(stem.gr.id) == FALSE) %>% # filter all stem that do not have stem group id
      select(treeid, plotcode, family, species, census.yr, d) %>% 
      rename(census.yr = census.yr,
             d = d)%>% 
      mutate(treeid = as.character(treeid),
             status = 1,
             n.stems = 1)
    
    total_census2 <- bind_rows(multi_census2, one_census2) %>% 
      mutate(census.n=2)
    
    full_data <- full_join(total_census1, total_census2)
    data <- full_data
    
    print("In case of the warning 
      'Caused by warning in `max()`[...]'
      do not worry! it means no multi-stemmed trees!")
  }
  
  if(metric == "carbon"){ # maintains the original, multi-stemmed dataset
    data <- data
  }
  
  # filter to separate survivors, recruits, or dead trees ----
  #census1
  #census2
  
  if(rate == "survival"){
    #census to clean = census2; baseline = census1
    #census1
    census1 = data %>% filter(census.n==1)
    #census2
    census2 = data %>% filter(census.n==2) 
    
    census_to_clean <- census2
    baseline <- census1
    z <- census_to_clean$treeid # list of treeids of data to be cleaned
    k <- baseline$treeid # treeid of baseline census
    rm <- setdiff(z, k) # save treeids present in census2 and absent in census1 to be removed
    census_to_clean <- census_to_clean[!z %in% rm,] # remove those treeids - probably recruits
    k <- census_to_clean$treeid # list of treeids of data to be cleaned
    z <- baseline$treeid # treeid of baseline census
    rm <- setdiff(k,z) # save treeids present in c1 and absent in c2 to be removed 
    census_to_clean <- census_to_clean[!k %in% rm,]  # remove those treeids - probably dead
    
    # filter only stems in both censuses
    census1.f <-  census1 %>% filter(treeid %in% census_to_clean$treeid)
    
    # combine datasets and information of alive (1) and survivor
    survivors <- bind_rows(census1.f,census_to_clean) %>% 
      mutate(status = "1", # status == 1 means alive
             descricao = "survivor")
    
    # join census date info
    survivors <- inner_join(survivors, dates)
    
    # calcualte time census interval
    census_to_clean <- survivors %>% 
      mutate(time.interv = (census.yr2-census.yr1))
  }
  
  if(rate=="mortality"){ 
    #census to clean = census1; baseline = census2
    #census1
    census1 = data %>% filter(census.n==1)
    #census2
    census2 = data %>% filter(census.n==2) 
    
    census_to_clean <- census1
    baseline <- census2
    z <- census_to_clean$treeid # list of treeids of data to clean, census 1
    k <- baseline$treeid #treeid of baseline census
    rm <- setdiff(z, k) # save treeids present in census1 and absent in census2
    census_to_clean <- census_to_clean[z %in% rm,] # retain treeids in census1 absent in census2
    
    # combine datasets and information of dead (0) and dead
    census_to_clean <- census_to_clean %>% 
      mutate(status = "0", # status == 0 means dead
             descricao = "dead")
    # calculate time census interval
    census_to_clean <- inner_join(census_to_clean, dates) %>% 
      mutate(time.interv = (census.yr2-census.yr1))
  }
  
  if(rate=="recruitment") { 
    #census to clean = census2; baseline = census1
    #census1
    census1 = data %>% filter(census.n==1)
    #census2
    census2 = data %>% filter(census.n==2) 
    
    census_to_clean <- census2
    baseline <- census1
    z <- census_to_clean$treeid # list of treeids of data to clean, census 2
    k <- baseline$treeid # treeid of baseline census 1
    rm <- setdiff(z, k) # save treeids present in census 2 and absent in census 1
    
    # filter only stems in second census
    census_to_clean <- census_to_clean[z %in% rm,] #retain the treeids in census 2 absent in census 1, probably recruits
    
    # combine datasets and information of alive (1) and recruit
    census_to_clean <- census_to_clean %>% 
      mutate(status = "1",
             descricao = "recruit")
    
    # calculate time census interval
    census_to_clean <- inner_join(census_to_clean, dates) %>% 
      mutate(time.interv = (census.yr2-census.yr1))
  }
  
  return(census_to_clean)
} 

# usage: ------------------------------------------------------------------ 
# survival.vital <- demography(data = data, metric = "vital", rate = "survival", census.n = "1_2")
# recruitment.vital <- demography(data = data, metric = "vital", rate = "recruitment", census.n = "1_2")
# mortality.vital <- demography(data = data, metric = "vital", rate = "mortality", census.n = "1_2")
# survival.carbon <- demography(data = data, metric = "carbon", rate = "survival", census.n = "1_2")
# recruitment.carbon <- demography(data = data, metric = "carbon", rate = "recruitment", census.n = "1_2")
# mortality.carbon <- demography(data = data, metric = "carbon", rate = "mortality", census.n = "1_2")