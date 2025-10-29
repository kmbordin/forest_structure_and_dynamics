---
author: "Kauane Maiara Bordin"
date: "2025-10-28"
---

# Forest structure and dynamics: standard codes 🌳

These codes aim to standardise the data organisation and obtain structural and dynamics' parameters of forest communities. Forest structural parameters include analyses related to carbon storage and individual density, as well as taxonomic and functional diversity and composition. Forest dynamics' parameters, correspond to individual and carbon dynamics.

## Data for running the codes

Data must be organised into tables where variables are always in columns and observations are in rows. The following table shows an example. If the data is not organised this way, there is a package under development called *Data Harmonization*, by Alliance for Tropical Forest Science ([<https://github.com/Alliance-for-Tropical-Forest-Science/DataHarmonization>])

## Loading dataset

##### **\*important\*: the variables *must*** follow these names and column orders

```{text, message=FALSE}
variables   description                     format
plotcode    unique plot name                factor 
plot.area   plot area in hectares           numeric    
census.n    census number                   numeric
census.yr   census year                     numeric                  
treeid      unique stem number              numeric
stem.gr.id  stem to individualcommon code   character
species     species name without authorship character   
d           diameter in cm                  numeric
genus       genus without authorship        character            
family      species family                  character
latitude    plot latitude                   numeric
longitude   plot longitude                  numeric


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

```

## Loading packages

```{text, message=FALSE}
install.packages(BIOMASS);library(BIOMASS) # to estimate carbon
install.packages(tidyverse);library(tidyverse) # data organisation
install.packages(hillR);library(hillR) # diversity metrics
install.packages(vegan);library(vegan) # diversity metrics
install.packages(FD);library(FD) # diversity metrics
```

## Loading functions

```{text, message=FALSE}
# load functions ------
source("R/correct.zombie.R")   # code 1
source("R/correct.diameter.R") # code 2
source("R/carbon.est.R")       # code 3
source("R/demography.R")       # code 4
source("R/unmatch.stems.R")    # code 5, not required, but recommended 
source("R/forest.dynamics.R")  # code 6
source("R/all.diversity.R")    # code 7
```

# Brief description of each function

## \# code 1: `correct.zombie.R`:

Description: Sometimes long-term monitoring fails in tracking tree mortality and trees assigned as dead can 're-appear' alive in the next census: they are called *zombie-trees*. In this code we fix this issue by adding a measurement for the stem that is alive in census1, dead in census2, and alive again in census3. We use the mean dbh between census1 and census3 as stem dbh in census2.

## \# code 2: `correct.diameter.R`

Description: Sometimes long-term monitoring fails in tracking stem diameter and we observe abnormal absolute growth rates (positive or negative). In this code we fix this issue by setting a limit of maximum growth rates (positive == 4 cm/yr, negative == -0.5cm/yr) for each stem. If the stems show abnormal growth, the corrections applied here assume 0 growth rates to avoid any kind of bias. If the stem shows positive abnormal growth, we set dbh in census 2 as default; If the stem shows negative abnormal growth, we set dbh in census 1 as default.

## \# code 3: `carbon.est.R`

Description: biomass (AGB) and carbon (AGC) estimates using data from different sources. In this code we can obtain AGB and AGC using refined data of WD and individual height but also WD from global databases (Zanne et al 2014) and *E* parameter (environmental stress). The parameters allow to use both sources as well - local WD and E, or WD from databases and individual height. It is strongly advised to run `correct.diameter.R` function before to correct for any inconsistencies in dbh measurement.

The equation used to estimate biomass refers to the equation by Chave et al. 2014 (doi.org/10.1111/gcb.12629), which uses information on diameter, wood density, and an environmental stress component *E* (or individual height). From the information on species identity and location (latitude and longitude) we obtain wood density and *E*, respectivelly. The estimated value is in biomass (AGB, in Mg), which is converted to carbon (AGC, in Mg) using the conversion factor 0.456, indicated in Martin et al. 2018 (doi.org/10.1111/gcb.12629).

WD.info: TRUE or FALSE; If WD.info == TRUE, the species and WD values must be provided; If WD.info == FALSE, WD will be obtained from the global database (Zanne et al 2014).

H.info: TRUE or FALSE; If H.info == TRUE, a vector of individual height must be provided; If H.info == FALSE, the code will use the *E* (environmental stress) parameter from Chave et al (2014) and a vector of latitude and longitude must be provided.

## \# code 4: `demography.R`

Description: separates the dataset into survivors, recruits, and deads, for both vital rates (individual-level) and carbon dynamics (stem-level). In this code we can obtain information of survival, recruitment, and mortality to calculate forest dynamics and demographic metrics. Requires the dataset derived from `carbon.est.R` function; metric: "vital" (individual-level) or "carbon" (stem-level); rate: "survival", "recruitment", or "mortality"; census.n: "1_2", "2_3" ... "7_8".

## \# code 4: `unmatch.stems.R`

Description: Datasets sometimes may show some inconsistencies on treeids; for instance, they are not unique as they should be, which impacts the estimation of carbon gains due to growth of survivors. In this code we can correct this issue by filtering repeated treeids to match them and further calculate the carbon gains due growth of survivors. Requires the dataset of survivors, metric='carbon' derived from `demography.R` function.

## \# code 5: `forest.dynamics.R`

Description: calculates demographic rates (recruitment, mortality, turnover, individual-based) and forest dynamics (carbon productivity and losses, stem-based). Demographic rates are based on equations from Kohyama et al 2017 (doi.org/10.1111/2041-210X.12929, equations 5 and 7) and Phillips et al 2008 Science (doi.org/10.1126/science.282.5388.439). Demographic rates require the dataset os survivors, recruits, and deads, metric='vital' derived from `demography.R` function. Forest dynamics are based on equations from Kohyama et al 2019 (doi.org/10.1016/j.foreco.2018.11.010, equations 5 and 6). Forest dynamics requires the dataset os survivors, recruits, and deads, metric='carbon' derived from `demography.R` function.

```{text, message=FALSE}
 Output from the forest.dynamics.R function
 
 if metric = *carbon*
 "plotcode" - unique plot code
 "Bs0" - AGC of survivors in census 1 (t0)
 "B0" - AGC in census 1 (t0)
 "Bt" - AGC census 2 (t1)
 "t" - time census interval in years
 "prop.large" - proportion of trees >= 30cm dbh
 "n.stems.c2" - number of stems at census 2
 "n.stems.c2.ha" - number of stems at census 2 per hectare
 "gains" - gains due to growth of survivors + recruits (Mg/ha)
 "incr.surv" - gains due to growth of survivors (Mg/ha/yr)
 "basal.area.total.basal.area_c1" - basal area (m2/ha) in census 1
 "basal.area.total.basal.area_c2" - basal area (m2/ha) in census 2
 "losses" - carbon losses due to mortality (Mg/ha/ano)
 "ba.mort" - basal area (cm2/ha) of dead stems
 "recruit.incr.ha.yr" - gains due to recrtuitment (Mg/ha/yr) 
 "ba.rec" - basal area (cm2/ha) of recruited stems
 "AGC.ha.census1" - AGC in census 1 (Mg/ha)
 "AGC.ha.census2" - AGC in census 2 (Mg/ha)
 "Pann" - Annual productivity (Mg/ha/yr)
 "Lann" - Annual carbon losses (Mg/ha/yr)
 "net.carbon.change" - Pann-Lnn; net carbon change (Mg/ha/yr)
 "latitude" - latitude in decimal degrees
 "longitude" - longitude in decimal degrees
 
 if metric = *vital*
 "plotcode" - unique plot code
 "N0" - Initial number of individuals in census 1 (t0)
 "Nt" - Final number of individuals in census 2 (t1)
 "Nst" - Number of surviving individuals in census 2 (t1)
 "t" - time census interval in years
 "plot.area" - plot area in hectares
 "N.ind.inicial" - Initial number of individuals in census 1 (t0)
 "N.ind.inicial.ha" - Initial number of individuals in census 1 (t0) per hectare
 "N.ind.final" - Final number of individuals in census 2 (t1)
 "N.ind.final.ha" - Final number of individuals in census 2 (t1) per hectare
 "ma" - mortality rates (%/yr) 
 "raf" - recruitment rates (%/yr)
 "turnover" - mean values between mortality and recruitment rates (yr) 
```

## \# code 6: `all.diversity.R`

Description: taxonomic and functional diversity and composition. In this code we calculate taxonomic diversity (rarefied richness, q-order from Chao et al. 2014 (doi.org/10.1890/13-0133.1) (q0 = richness, q1 = Shannon index, q2 = Simpson index)), functional diversity (RaoQ, FRic, FEve, FDiv), taxonomic composition (NMDS), and functional composition (CWM). MANDATORY: please check the species' names in both trait and demographic tables; they *must* match; to provide the trait information is also mandatory.

```{text, message=FALSE}
Output from the all.diversity.R function - five lists

taxonomic.diversity list
    rich.dens.census1 - species richness in census 1 (q0)
    shannon.dens.census1 - Shannon index in census 1 (q1)
    inv.simpson.dens.census1 - Simpson index in census 1 (q2)
    rich.dens.census2 - species richness in census 2 (q0)
    shannon.dens.census2 - Shannon index in census 2 (q1)
    inv.simpson.dens.census2 - Simpson index in census 2 (q2)
    rarefied.richness.c1 - rarefied richness in census 1
    rarefied.richness.c2 - rarefied richness in census 1

community_matrix_c1 list
    Community matrix descrived by species abundance per plot in census 1

community_matrix_c2 list
    Community matrix descrived by species abundance per plot in census 2  
  
functional_census1 list
    nbsp: number of species (richness) in census 1
    sing.sp: functionally singular species per plot in census 1
    FRic: functional richness in census 1
    qual.FRic: quality of the reduced-space to compute FRic and FDiv in census 1
    FEve: functional eveness in census 1
    FDis: functional dispersion in census 1
    RaoQ: functional diversity (RaoQ) in census 1
    CWM: weighted mean traits per plot in census 1

functional_census2 list
    nbsp: number of species (richness) in census 2
    sing.sp: functionally singular species per plot in census 2
    FRic: functional richness in census 2
    qual.FRic: quality of the reduced-space to compute FRic and FDiv in census 2
    FEve: functional eveness in census 1
    FDis: functional dispersion in census 2
    RaoQ: functional diversity (RaoQ) in census 2
    CWM: weighted mean traits per plot in census 2
```

See the `example.of.use.R` script for an example of sequence to run all codes and their outputs.

If you find **any** inconsistency in the codes **please** contact kauanembordin[at]gmail.com :) 🌳

# \-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\--

Most recent news! I have been playing to develop a shiny.app to run these codes quick and easily. It is a very (*very)* simple version, but it is working for the default parameters so far. I will keep updating it when I have some spare time :)

In the shiny app there is an extra parameter `ontogeny`, where: juvenile == \<5cm dbh; adult == \>=5cm dbh; and all == all dbhs in the data.frame.
