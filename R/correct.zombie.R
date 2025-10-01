############################################################
##############  CORRECT ZOMBIE-TREES  ######################
############################################################
### Code developed by Kauane M Bordin at 01 Oct 2025

### Description: Sometimes long-term monitoring fails in tracking tree mortality
# and trees assigned as dead can 're-appear' in the next census: they are called 
# zombie-trees

### In this code we fix this issue by a register for the stem that is alive in census1
# dead in census2, and alive again in census3. We use the mean dbh between census1 and
# census3 as stem dbh in census2. 

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

correct.zombie <- function (data){
  
  #' @description generates correction for stems assigned as alive, dead, alive
  #' @author Kauane Maiara Bordin (kauanembordin[at]gmail.com)
  #' @param  data frame with three censuses
  #' @return data frame with zombie-trees corrected
  #' data: stem-level information from ForestPlots.net format, but be careful with the required format for column names
  
  census.three <- data %>%
    filter(census.n >=3) %>% # filter plots with three or more censuses
    dplyr::select(plotcode) %>%
    unique()
  
  dataset <- data %>%
    filter(plotcode %in% census.three$plotcode) # filter data from plots with three or more censuses
  
    result <- dataset %>% # combine the treeids according to the census they occur
    group_by(treeid) %>%
    summarise(census_set = paste(sort(unique(census.n)), collapse = ","), .groups = "drop") %>%
    mutate(status = case_when(census_set == "1,2,3" ~ 1,  # censuses 1, 2 and 3
                              census_set == "1,3" ~ 1,  # censuses em 1 e 3
                              census_set == "2,3" ~ 1,  # censuses em 2 e 3
                              census_set == "1" ~ 0,  # census 1 only
                              census_set == "2" ~ 0,  # census 2 only
                              census_set == "3" ~ 1)) %>%   # census 3 only
    right_join(dataset, by = "treeid")
    
  # If there is a tree with status alive, dead, alive, they will receive a new line with the missing census info, and mean dbh, so they will become alive, alive, alive
  correct <-  result %>%
    filter(census_set == "1,3") %>%
    group_by(plotcode,plot.area,treeid,stem.gr.id,species,genus,family,latitude,longitude) %>%
    summarise(census.n = 2,
              census.yr = NA,
              d = mean(d, na.rm = TRUE), # set the mean dbh from census1 and census3 for census2
              .groups = "drop") %>%
    ungroup()

  # combine the datasets
  result <- dataset %>%
    bind_rows(correct)  %>%
    group_by(plotcode, census.n) %>%
    mutate(census.yr = ifelse(is.na(census.yr) & census.n == 2,
                              yes = first(census.yr[!is.na(census.yr)]), # fill the census year of second census for the plot
                              no = census.yr)) %>%
    ungroup()
  return(result)
}

# usage: ------------------------------------------------------------------ 
# data.correct.for.zombie <- correct.zombie (data = data)

