library(shiny)
library(zip)

ui <- fluidPage(tags$head(
  tags$style(HTML("
    body { background-color: azure; }
    .well { background-color: azure2; border: 1px solid grey70; }
    .btn { background-color: azure2; border-color: grey70; }
  "))),
  titlePanel("App para obter estimativas de estrutura e dinâmica florestal - LERSAF (UFRGS)"),
  sidebarLayout(
    sidebarPanel(
      actionButton("run", "Rodar Funções"),
      br(), br(),
      downloadButton("download_zip", "Baixar Todos os Resultados + Funções (.zip)"),
      selectInput("ontogeny",label = "ontogenetic stage",
                  choices = c("juvenile", "adult","all"), selected = "all"),
      selectInput("census.n",label = "correct.census and demography: census.number:",
        choices = c("1_2", "2_3", "3_4", "4_5", "6_7","7_8"), selected = "1_2"),
      numericInput("dbh", "correct.census and carbon estimates: dbh:", value = 10, min = 0),
      selectInput("WD.info",label = "carbon.estimation: WD.info:",
                  choices = c("TRUE", "FALSE"), selected = "TRUE"),
      selectInput("H.info",label = "carbon.estimation: H.info:",
                  choices = c("TRUE", "FALSE"), selected = "TRUE"),
      #selectInput("census.n",label = "demography: census.number:",
                  #choices = c("1_2", "2_3", "3_4", "4_5", "6_7","7_8"), selected = "1_2"),
      selectInput("metric",label = "demography: metric:",
                  choices = c("vital", "carbon"), selected = "vital")),
    mainPanel(
      uiOutput("resultados")
    )
  )
)

server <- function(input, output) {
  
  # Dataset de exemplo
  dados_iniciais <- data.complete
  trait.data <- trait
  minhas_funcoes <- list(
    ontogeny = function(data, ontogeny){
      if(ontogeny == "juvenile"){
        data <- data %>%
          filter(d>0) %>% 
          filter(d<5)
      }
      if(ontogeny == "adult"){
        data <- data %>% 
          filter(d>=5)
      }
        if(ontogeny == "all"){
          data <- data
        }
      return(data)
    },
    correct.zombie = function (data){
      
      #' @description generates correction for stems assigned as alive, dead, alive
      #' @author Kauane Maiara Bordin (kauanembordin[at]gmail.com)
      #' @param  data frame with three censuses
      #' @return data frame with zombie-trees corrected
      #' data: stem-level information from ForestPlots.net format, but be careful with the required format for column names
      
      census.three <- data %>%
        filter(census.n >=3) %>% # filter plots with three or more censuses
        dplyr::select(plotcode) %>%
        unique()
      census.two <- data %>%
        filter(!plotcode %in% census.three$plotcode) %>% # filter plots with less than three  censuses
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
      
      # If there is a tree with status alive, dead, alive (1,3), they will receive a new line with the missing census info, and mean dbh, so they will become alive, alive, alive
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
                                  yes = first(census.yr[!is.na(census.yr)]), 
                                  no = census.yr)) %>% # fill the census year of second census for the plot
        ungroup() %>% 
        bind_rows(census.two)
      
      return(result)
    } ,
    correct.diameter = function (data,census.n,dbh, WD.info, H.info){
      
      #' @description correct diameter to avoid abnormal growth rates
      #' @author Kauane Maiara Bordin (kauanembordin[at]gmail.com)
      #' @param  data frame with stem-level data 
      #' @return data frame with diameter corrected for abnormal growth
      #' census.number: 1 == applies for all available censuses; 2, 3 or 4: filters census 2 to 4.
      #' data: stem-level information from ForestPlots.net format, but be careful with the required format for column names
      #' census.n: "1_2", "2_3" ... "6_7"
      #' dbh: minimum dbh to apply the correction - please check your data! here dbh is in cm 
      
      ferns.families <- c("Cyatheaceae", "Dicksoniaceae","Pteridaceae") #fern families to be removed
      
      data <- data %>% 
        filter(d >= dbh) %>%  # to ensure same dbh threshold for all stems. dbh must be in cm!
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
        data = bind_rows(census1,census2)
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
      if(census.n=="7_8"){ # data filtering for census 6 and 7
        #census1
        census1 = data %>% filter(census.n == 7) %>% 
          mutate(census.n = replace(census.n, census.n == "7", "1")) 
        #census2
        census2 = data %>% filter(census.n == 8) %>% 
          mutate(census.n = replace(census.n, census.n == "8", "2")) 
        c2 = unique(census2$plotcode)
        census1 <- census1 %>% 
          filter(plotcode %in% c2)
        data = bind_rows(census1,census2)
      }
      
      # The code below aims to summarise the census, species, and plot information
      
      interval <- data %>%
        ungroup() %>% 
        dplyr::select(plotcode,census.n, census.yr) %>% 
        group_by(plotcode, census.n) %>%
        mutate(original.census = original.census.n,
               row_id = row_number()) %>%  # identify duplicate values
        pivot_wider(names_from = census.n, # pivot table to allow summarisation of plots and censuses
                    values_from = census.yr,
                    names_prefix = "census",
                    values_fn = list(census.yr = list)) %>%  
        unnest(cols = everything()) %>%  
        dplyr::select(-row_id) %>%  # remove id of duplicates
        unique() %>% 
        drop_na() %>% 
        mutate(census.interv = census2-census1) %>% # calculate census interval in years
        dplyr::select(-c(census2,census1))
      
      # summarise plot information
      data.summarised <- data %>% 
        ungroup() %>% 
        dplyr::select(plotcode,plot.area,latitude,longitude) %>% 
        unique()
      
      # summarise census information
      data.summarised2 <- data %>% 
        ungroup() %>% 
        dplyr::select(plotcode,census.yr,census.n) %>% 
        unique() %>% 
        group_by(plotcode) %>% 
        pivot_wider(id_cols = NULL,names_from = census.n ,values_from = census.yr) %>% 
        rename(census.number1 = `1`,
               census.number2 = `2`)
      if(H.info==TRUE){
      overall.height.mean <- mean(data$Height, na.rm = TRUE)
      non_na_heights <- data$Height[!is.na(data$Height)]
      
      if (length(non_na_heights) > 0) {
        overall.height.mean <- mean(non_na_heights)
      } else {
        overall.height.mean <- NA }
      }
      #summarise species information

      if(H.info==FALSE){
        data <- data %>% 
        mutate(Height = NA)
      }
      if(WD.info==TRUE){
        data <- data
      }
      if(WD.info==FALSE){
        data <- data %>% 
          mutate(WD = NA)
      }

      if(WD.info==TRUE & H.info==TRUE){
        data.summarised3 <- data %>% 
          ungroup() %>% 
          dplyr::select(species,family,genus,WD,Height,treeid,census.n) %>% 
          ungroup() %>% 
          group_by(species) %>%
          mutate(WD = coalesce(WD, unique(WD[!is.na(WD)]))) %>% # includes the WD of former zombie tree
          ungroup() %>% 
          group_by(treeid) %>%
          mutate(Height1 = (mean(Height, na.rm = TRUE))) %>% # includes the mean height of former zombie tree
          mutate(Height = coalesce(Height1,overall.height.mean),
                 census.n = as.numeric(census.n)) %>% 
          unique()
      }
      if(WD.info==TRUE & H.info==FALSE){
        data.summarised3 <- data %>% 
          ungroup() %>% 
          dplyr::select(species,family,genus,WD,Height,treeid,census.n) %>% 
          ungroup() %>% 
          group_by(species) %>%
          mutate(WD = coalesce(WD, unique(WD[!is.na(WD)]))) %>% # includes the WD of former zombie tree
          ungroup() 
      }
      if(WD.info==FALSE & H.info==TRUE){
        data.summarised3 <- data %>% 
          ungroup() %>% 
          dplyr::select(species,family,genus,WD,Height,treeid,census.n) %>% 
          ungroup() %>% 
          group_by(treeid) %>%
          mutate(Height1 = (mean(Height, na.rm = TRUE))) %>% # includes the mean height of former zombie tree
          mutate(Height = coalesce(Height1,overall.height.mean),
                 census.n = as.numeric(census.n)) %>% 
          unique()
      }
      if(WD.info==FALSE & H.info==FALSE) {
        data.summarised3 <- data %>% 
        ungroup() %>% 
        dplyr::select(species,family,genus,WD,Height,treeid,census.n) %>% 
        unique()
      }
      
      # the following code effectively corrects for the dbh between census 
      # the corrections are applied to dbhs that present abnormal growth (negative or positive).
      # abnormal negative growth relates to growth rates lower than -0.5 cm/yr
      # abnormal positive growth is higher than 4 cm/yr.
      
      correct.dbh <- data %>% 
        dplyr:: select(plotcode, species, census.n, treeid, d, stem.gr.id,census.yr) %>%
        left_join(interval, by = c("plotcode")) %>% # unite census info
        group_by(plotcode, species, census.n, treeid, d, stem.gr.id, census.interv,original.census) %>% # large grouping to avoid any mistakes on tree growth correction
        summarise(d = mean(d, na.rm = TRUE), .groups = "drop") %>% 
        pivot_wider(names_from = c(census.n),
                    values_from = d, 
                    names_prefix = "census") %>% 
        relocate(census1, .before = census2) %>% 
        mutate(gr =(census2-census1)/census.interv) %>% # calculate absolute growth rates per year in cm
        mutate(gr2 = ifelse(gr < (-0.5), "ab.neg", gr), # test for abnormal negative growth -- if the absolute growth is lower than -0.5, set the value as -0.5 to be filtered next
               gr.corr = ifelse(gr2 > 4, "ab.pos", gr2), # test for abnormal positive growth -- if the growth is larger than 4, set the value as 4 to be filtered next
               d1.cor = ifelse(gr.corr == "ab.pos", census2, census1), # if census 2 has larger dbh than census 1, we determined dbh2 == dbh1 by setting dbh2 as default, thus dbh1 will be corrected to dbh1 == dbh2
               d2.cor = ifelse(gr.corr == "ab.neg", census1, census2),# if census 1 has larger dbh than census 2, we determined dbh1 == dbh2 by setting dbh1 as default, thus dbh2 will be corrected to dbh2 == dbh1
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
               census.yr = ifelse(census.n == max(census.n),census.number2, census.yr),
               census.n = as.numeric(census.n)) %>% 
        dplyr::select(-c(census.number1,census.number2,original.time1,original.time2,census)) %>% 
        drop_na(d) %>% # drop the NA values in dbh
        ungroup() %>%
        group_by(census.n,treeid,census.yr) %>% 
        left_join(data.summarised3, by = c("species","census.n","treeid")) %>% # unite species info
        dplyr::select(plotcode,plot.area,census.n,census.yr,treeid,stem.gr.id,species,d,genus,family,latitude,longitude,WD,Height) %>% 
        mutate(stem.gr.id = as.character(stem.gr.id))
      
      data <- correct.dbh # set the dataframe of corrected dbh as the default
      return(data)
    },
    carbon.est = function (data, WD.info, H.info, dbh){
      
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
        filter(! family %in% ferns.families) %>%  #remove fern families
        ungroup()
      
      if(WD.info==TRUE){
        WD <- data %>% 
          dplyr::select(species,WD)
      }
      
      if(WD.info==FALSE){  #get wood density values for species from Zanne et al 2014
        data <- data %>% dplyr::select(-WD)
        WD <- BIOMASS::getWoodDensity(data$genus, data$species) %>% 
          dplyr::select(species,meanWD,levelWD) %>% 
          rename(WD = meanWD) %>% 
          distinct(species, .keep_all = TRUE)
      }
      
      if(H.info==TRUE){
        H <- data %>% 
          dplyr::select(Height) 
      }
      
      if(H.info==FALSE){  #get wood density values for species from Zanne et al 2014
        H <- data %>% 
          ungroup() %>% 
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
          mutate(WD = WD.x)%>% 
          dplyr::select(-Height)
        biomass  <-  BIOMASS::computeAGB(D = data.c$d, WD = data.c$WD, coord = H)
        biomass <- data.frame(AGB = biomass) %>% 
          dplyr::mutate(AGC = AGB*0.456)  #estimate AGC based on Martin et al 2018
      }
      
      if(H.info==FALSE & WD.info==FALSE){
        WD <- WD %>% dplyr::select (species,WD) 
        data.c <- left_join(data,WD, by="species") %>% 
          dplyr::select(-Height)
        biomass  <-  BIOMASS::computeAGB(D = data.c$d, WD = data.c$WD, coord = H)
        biomass <- data.frame(AGB = biomass) %>% 
          dplyr::mutate(AGC = AGB*0.456)  #estimate AGC based on Martin et al 2018
      }
      
      if(H.info==TRUE & WD.info==FALSE){
        WD <- WD %>% dplyr::select (species,WD) 
        data.c <- left_join(data,WD, by="species") 
        biomass  <-  BIOMASS::computeAGB(D = data.c$d, WD = data.c$WD, H = H$Height)
        biomass <- data.frame(AGB = biomass) %>% 
          dplyr::mutate(AGC = AGB*0.456)  #estimate AGC based on Martin et al 2018
      }
      
      data.complete <- bind_cols(data,biomass)
      print(head(data.complete))
      return(data.complete)
    },
    demography = function(data, metric, census.n) {
      
      if(census.n=="1_2"){
        census1 = data %>% filter(census.n==1)
        census2 = data %>% filter(census.n==2)
        data = bind_rows(census1,census2)
      }
      
      if(census.n=="2_3"){
        census1 = data %>% filter(census.n==2) %>% mutate(census.n="1")
        census2 = data %>% filter(census.n==3) %>% mutate(census.n="2")
        c2 = unique(census2$plotcode)
        census1 <- census1 %>% filter(plotcode %in% c2)
        data = bind_rows(census1,census2)
      }
      
      if(census.n=="3_4"){
        census1 = data %>% filter(census.n==3) %>% mutate(census.n="1")
        census2 = data %>% filter(census.n==4) %>% mutate(census.n="2")
        c2 = unique(census2$plotcode)
        census1 <- census1 %>% filter(plotcode %in% c2)
        data = bind_rows(census1,census2)
      }
      
      if(census.n=="4_5"){
        census1 = data %>% filter(census.n==4) %>% mutate(census.n="1")
        census2 = data %>% filter(census.n==5) %>% mutate(census.n="2")
        c2 = unique(census2$plotcode)
        census1 <- census1 %>% filter(plotcode %in% c2)
        data = bind_rows(census1,census2)
      }
      
      if(census.n=="5_6"){
        census1 = data %>% filter(census.n==5) %>% mutate(census.n="1")
        census2 = data %>% filter(census.n==6) %>% mutate(census.n="2")
        c2 = unique(census2$plotcode)
        census1 <- census1 %>% filter(plotcode %in% c2)
        data = bind_rows(census1,census2)
      }
      
      if(census.n=="6_7"){
        census1 = data %>% filter(census.n==6) %>% mutate(census.n="1")
        census2 = data %>% filter(census.n==7) %>% mutate(census.n="2")
        c2 = unique(census2$plotcode)
        census1 <- census1 %>% filter(plotcode %in% c2)
        data = bind_rows(census1,census2)
      }
      
      if(census.n=="7_8"){
        census1 = data %>% filter(census.n==7) %>% mutate(census.n="1")
        census2 = data %>% filter(census.n==8) %>% mutate(census.n="2")
        c2 = unique(census2$plotcode)
        census1 <- census1 %>% filter(plotcode %in% c2)
        data = bind_rows(census1,census2)
      }
      
      date1 <- census1 %>% group_by(plotcode) %>% summarise(census.yr1 = mean(census.yr))
      date2 <- census2 %>% group_by(plotcode) %>% summarise(census.yr2 = mean(census.yr))
      dates <- inner_join(date1, date2)
      
      if(metric == "vital") {
        census1 <- data %>% filter(census.n == 1)
        multi_census1 <- census1 %>% filter(!is.na(stem.gr.id)) %>%
          group_by(stem.gr.id, plotcode, family, species) %>%
          summarise(census.yr = mean(census.yr),
                    n.stems = n(),
                    d = max(d)) %>%
          rename(treeid = stem.gr.id) %>%
          mutate(treeid = as.character(treeid))
        
        one_census1 <- census1 %>% filter(is.na(stem.gr.id)) %>%
          select(treeid, plotcode, family, species, census.yr, d) %>%
          mutate(treeid = as.character(treeid), n.stems = 1)
        
        total_census1 <- bind_rows(multi_census1, one_census1) %>%
          mutate(status = 1, census.n = 1)
        
        census2 <- data %>% filter(census.n == 2)
        multi_census2 <- census2 %>% filter(!is.na(stem.gr.id)) %>%
          group_by(stem.gr.id, plotcode, family, species) %>%
          summarise(census.yr = mean(census.yr),
                    d = max(d),
                    n.stems = n()) %>%
          rename(treeid = stem.gr.id) %>%
          mutate(treeid = as.character(treeid))
        
        one_census2 <- census2 %>% filter(is.na(stem.gr.id)) %>%
          select(treeid, plotcode, family, species, census.yr, d) %>%
          mutate(treeid = as.character(treeid), n.stems = 1)
        
        total_census2 <- bind_rows(multi_census2, one_census2) %>% mutate(census.n = 2)
        data <- full_join(total_census1, total_census2)
      }
      
      if(metric == "carbon") {
        data <- data
      }
      
      ## SURVIVAL
      survival <- {
        census1 = data %>% filter(census.n == 1)
        census2 = data %>% filter(census.n == 2)
        
        census_to_clean <- census2
        baseline <- census1
        z <- census_to_clean$treeid
        k <- baseline$treeid
        rm <- setdiff(z, k)
        census_to_clean <- census_to_clean[!z %in% rm,]
        k <- census_to_clean$treeid
        z <- baseline$treeid
        rm <- setdiff(k, z)
        census_to_clean <- census_to_clean[!k %in% rm,]
        
        census1.f <- census1 %>% filter(treeid %in% census_to_clean$treeid)
        survivors <- bind_rows(census1.f, census_to_clean) %>%
          mutate(status = "1", descricao = "survivor")
        inner_join(survivors, dates) %>% 
          mutate(time.interv = (census.yr2 - census.yr1))
      }
      
      ## MORTALITY
      mortality <- {
        census1 = data %>% filter(census.n == 1)
        census2 = data %>% filter(census.n == 2)
        
        census_to_clean <- census1
        baseline <- census2
        z <- census_to_clean$treeid
        k <- baseline$treeid
        rm <- setdiff(z, k)
        census_to_clean <- census_to_clean[z %in% rm,]
        
        census_to_clean %>%
          mutate(status = "0", descricao = "dead") %>%
          inner_join(dates) %>%
          mutate(time.interv = (census.yr2 - census.yr1))
      }
      
      ## RECRUITMENT
      recruitment <- {
        census1 = data %>% filter(census.n == 1)
        census2 = data %>% filter(census.n == 2)
        
        census_to_clean <- census2
        baseline <- census1
        z <- census_to_clean$treeid
        k <- baseline$treeid
        rm <- setdiff(z, k)
        census_to_clean <- census_to_clean[z %in% rm,]
        
        census_to_clean %>%
          mutate(status = "1", descricao = "recruit") %>%
          inner_join(dates) %>%
          mutate(time.interv = (census.yr2 - census.yr1))
      }
      
      return(list(
        survival = survival,
        recruitment = recruitment,
        mortality = mortality))},
    unmatch.stems = function(data.survival.carbon){

      #' @description corrects for duplicate treeids
      #' @author Kauane Maiara Bordin
      #' @param data dataframe with stem level data of survivors
      #' @return dataframe with matching survivors between censuses
      #' data.survival.carbon: data of survivors derived from demography function, metric='carbon'

      survc <- data.survival.carbon
      surv_c1 <- survc %>% filter(census.n == 1) %>% arrange(treeid)
      surv_c2  <-  survc %>% filter(census.n == 2) %>% arrange(treeid)

      n_occur.surv1 <- surv_c1 %>%
        group_by(treeid) %>%
        summarise(Freq = n()) %>%
        filter(Freq > 1) # counts and filters repeated treeids in census1
      n_occur.surv2 <- surv_c2 %>%
        group_by(treeid) %>%
        summarise(Freq = n()) %>%
        filter(Freq > 1) # counts and filters repeated treeids in census2

      # removing duplicated treeids
      surv_c1 <- surv_c1 %>%
        filter(! treeid %in% n_occur.surv1$treeid)
      surv_c2 <- surv_c2 %>%
        filter(! treeid %in% n_occur.surv1$treeid)
      surv_c1 <- surv_c1 %>%
        filter(! treeid %in% n_occur.surv2$treeid)
      surv_c2 <- surv_c2 %>%
        filter(! treeid %in% n_occur.surv2$treeid)

      survc <- bind_rows(surv_c1,surv_c2)
      return(survc)
    },
    demography.and.dynamics = function (survival,mortality,recruitment,metric){
      
      #' @description generate data for demographic rates and forest dynamics
      #' @author Kauane Maiara Bordin
      #' @param  data frame with stem (carbon) or individual (vital) level data across censuses
      #' @return data frame with demographic rates (recruitment, mortality, turnover) or
      #'         forest dynamics (productivity and losses)
      #' survival: derived from demography function
      #' mortality: derived from demography function
      #' recruitment: derived from demography function
      #' metric: "vital" or "carbon"
      
      # function to calculate basal area in cm2/ha
      basal.area<- function(x) { # x is dbh in cm
        (pi*(x/2)^2)/10000} #to cm2/ha
      
      mort <- mortality
      rec <- recruitment
      surv <- survival
      
      # save plot information
      plots <- unique(surv$plotcode)
      print(setdiff(plots,mort$plotcode))# plots with zero mortality
      print(setdiff(plots,rec$plotcode)) # plots with zero recruitment
      
      if(metric == "carbon"){
        print("METRIC == CARBON")
        
        # spatial location of each plot 
        gps <- surv %>% 
          dplyr::select(plotcode,latitude,longitude) %>% unique()
        
        # time census interval in years 
        time.interv <- surv %>% 
          dplyr::select(plotcode,time.interv) %>% 
          unique() %>% arrange(plotcode)
        
        # summarising mortality data -----
        # AGC and basal area of dead stems per ha e per ha/yr in census2
        # it is based on stem AGC in census 1, when they were alive
        mort  <-  mort %>% 
          mutate(mort.agc.total.ha = AGC/plot.area, # stem AGCmort per hectare
                 mort.agc.total.ha.yr = mort.agc.total.ha/time.interv, # stem AGCmort per hectare per year
                 status = as.numeric(status), # might be 0
                 ba = basal.area(d)) # calculate basal area
        
        # plot-level AGC and basal area per ha of dead stems in census2
        # it is based on AGC in census 1, when the stems were alive
        mort.agc.total.ha <- mort %>% 
          group_by(plotcode) %>%
          summarise(mort.agc.total.ha = sum(mort.agc.total.ha),
                    ba.mort = sum(ba))  
        
        # summarising recruitment data ------
        # AGC and basal area of recruited stems per ha and per ha/yr in census2
        # it is based on stem AGC in census 2, when they were alive
        rec <- rec %>% 
          mutate(rec.agc.total.ha = AGC/plot.area, # stem AGCrec per hectare
                 rec.agc.total.ha.yr = rec.agc.total.ha/time.interv, # stem AGCrec per hectare per year
                 status = as.numeric(status), #might be 1
                 ba = basal.area(d)) # calculate basal area
        
        # plot-level AGC and basal area per ha/year of recruited stems in census2
        # it is based on AGC in census 2, when the stems were alive
        rec.agc_gain.ha.yr = rec %>% 
          group_by(plotcode) %>%
          summarise(rec.ha.yr = sum(rec.agc.total.ha.yr),
                    ba.rec = sum(ba)) 
        
        # plot-level AGC and basal area of recruited stems per ha in census2
        # it is based on AGC in census 2, when the stems were alive    
        rec.agc.total.ha.c2  <- rec %>% 
          group_by(plotcode) %>%
          summarise(agc.total.rec.ha.c2 = sum(rec.agc.total.ha),
                    ba.rec = sum(ba)) 
        
        # summarising survival data ------
        surv <- surv %>% 
          mutate(status = as.numeric(status), # might be 1
                 ba = basal.area(d))
        
        #filter census1 and order treeid
        surv_c1 = surv %>% filter(census.n == 1) %>% arrange(treeid)
        
        #filter census2 and order treeid
        surv_c2  <-  surv %>% filter(census.n == 2) %>% arrange(treeid)
        
        # new df to calculate growth
        growth <- data.frame(plotcode = surv_c1$plotcode,
                             plot.area = surv_c1$plot.area,
                             treeid = surv_c1$treeid, # must match surv_c2$treeid. if not, run the 'unmatch.stems' function
                             treeid.check = surv_c2$treeid, # must match surv_c1$treeid. if not, run the 'unmatch.stems' function
                             species = surv_c1$species,
                             census1 = surv_c1$census.yr,
                             census2 = surv_c2$census.yr,
                             AGCt1 = surv_c1$AGC,
                             AGCt2 = surv_c2$AGC, 
                             status = surv_c2$status, # must be 1
                             time.interv = surv_c2$time.interv,
                             ba.surv.t1 = surv_c1$ba,
                             ba.surv.t2 = surv_c2$ba) %>% 
          mutate(agc_gain = AGCt2-AGCt1,  # gain due growth in AGC
                 agc_gain.ha = agc_gain/plot.area,  # growth in AGC per ha
                 agc_gain.yr = agc_gain/time.interv, # growth in AGC per year
                 #standardise the gains in carbon for hectare per year
                 agc_gain.ha.yr = agc_gain.yr/plot.area,
                 #standardise carbon storage in census1 to hectare
                 agc.total.surv.ha.c1 = AGCt1/plot.area,
                 #standardise carbon storage in census2 to hectare
                 agc.total.surv.ha.c2 = AGCt2/plot.area,
                 status = as.numeric(status))
        
        # gains in carbon due to growth per ha, ha per yr, 
        # and total AGC in the first (Bs0, carbon of survivors in  census 1) and second census
        surv.agc  <- growth %>% group_by(plotcode) %>%
          summarise(surv.agc_gain.ha.c2 = sum(agc_gain.ha),
                    surv.agc_gain.ha.yr = sum(agc_gain.ha.yr),
                    agc.total.surv.ha.c1 = sum(agc.total.surv.ha.c1), # this is Bs0
                    agc.total.surv.ha.c2 = sum(agc.total.surv.ha.c2),
                    ba.surv.t1 = sum(ba.surv.t1),
                    ba.surv.t2 = sum(ba.surv.t2)) %>% 
          mutate(Bs0 = agc.total.surv.ha.c1)
        
        # only gains through growth and recruitment per ha
        all.carbon.gains.c2 <-  surv.agc %>% 
          dplyr::select(plotcode, surv.agc_gain.ha.c2) %>% 
          full_join (rec.agc.total.ha.c2) %>% 
          mutate(across(agc.total.rec.ha.c2, coalesce, 0)) %>% # in case there is zero recruitment
          mutate(gains = (surv.agc_gain.ha.c2)+(agc.total.rec.ha.c2)) 
        
        # inital carbon in census 1 (B0, for all stems in census 1)
        all.carbon.c1 <- surv.agc %>% 
          dplyr::select(plotcode,agc.total.surv.ha.c1) %>% 
          full_join (mort.agc.total.ha) %>%  
          mutate(across(mort.agc.total.ha, coalesce, 0)) %>% # in case there is zero mortality
          mutate(B0 = (agc.total.surv.ha.c1)+(mort.agc.total.ha)) 
        
        # total carbon in census 2 (Bt, for survivors, their growth, and recruitment)
        all.carbon.c2 <- surv.agc %>% 
          dplyr::select(plotcode,agc.total.surv.ha.c1,surv.agc_gain.ha.c2) %>% 
          full_join(rec.agc.total.ha.c2) %>% 
          mutate(across(agc.total.rec.ha.c2, coalesce, 0)) %>% # in case there is zero recruitment
          mutate(Bt = (agc.total.surv.ha.c1)+(surv.agc_gain.ha.c2)+(agc.total.rec.ha.c2))
        
        #time census interval (t)
        time <- surv %>% 
          dplyr::select(plotcode,time.interv) %>% 
          unique() %>% 
          arrange(plotcode)
        
        basal.area.total <- data.frame(plotcode = surv_c1$plotcode) %>% 
          full_join(surv.agc, by="plotcode")%>% 
          full_join(mort.agc.total.ha, by="plotcode")%>% 
          full_join(rec.agc.total.ha.c2, by="plotcode")%>% 
          unique() %>% 
          mutate(across(where(is.numeric), tidyr::replace_na, 0)) %>% #in case there is na
          dplyr::select(plotcode, ba.surv.t1,ba.surv.t2,ba.mort,ba.rec) %>% 
          mutate(basal.area_c1 = (ba.surv.t1 + ba.mort),
                 basal.area_c2 = (ba.surv.t2 + ba.rec))
        
        #final carbon tables ------
        carbon_estimates_per_plot <- data.frame(Bs0 = surv.agc$Bs0,
                                                B0 = all.carbon.c1$B0,
                                                Bt = all.carbon.c2$Bt,
                                                t = time$time.interv,
                                                plotcode = time$plotcode,
                                                gains = all.carbon.gains.c2$gains, # survivors' growth + recruitment
                                                incr.surv = surv.agc$surv.agc_gain.ha.yr, # survivors' growth per ha/yr
                                                basal.area.c1 = basal.area.total$basal.area_c1,
                                                basal.area.c2 = basal.area.total$basal.area_c2) %>% 
          left_join(mort.agc.total.ha) %>% 
          left_join(rec.agc_gain.ha.yr) %>%
          dplyr::rename(losses = mort.agc.total.ha,
                        recruit.incr.ha.yr = rec.ha.yr) %>% 
          relocate(plotcode, .before = Bs0) %>% 
          mutate(across(where(is.numeric), tidyr::replace_na, 0)) %>% #in case there is NA, replace to 0
          dplyr::mutate(AGC.ha.census1 = B0,
                        AGC.ha.census2 = Bt,
                        Pann = ((1-((Bs0/Bt)^(1/t)))*(Bt-B0))/((t*(1-(B0/Bt)^(1/t)))), # Kohyama  eq 3
                        Lann = ((1-((Bs0/B0)^(1/t)))*(Bt-B0))/((t*(((Bt/B0)^(1/t))-1))), # Kohyama eq 4
                        net.carbon.change = Pann-Lann) %>% 
          mutate(across(losses, coalesce, 0)) %>% # in case this is zero
          mutate(across(recruit.incr.ha.yr, coalesce, 0)) %>% # in case this is zero
          mutate(across(everything(), ~ ifelse(is.nan(.), 0, .)))
        
        estimates <- carbon_estimates_per_plot %>% 
          left_join(gps, by="plotcode")
        
      }
      #-------------------------------------------------------------------------------------
      if(metric == "vital"){
        print("METRIC == VITAL RATES")
        
        # time census interval (t)
        time <- surv %>% ungroup() %>% 
          dplyr::select (plotcode,time.interv) %>% 
          unique() %>% 
          arrange(plotcode)
        
        # number of survival individuals in the second census (Nst)
        surv.n.c2 <- surv %>% 
          filter(census.n == 2)%>% 
          group_by(plotcode) %>%
          mutate(status = as.numeric(status)) %>% 
          summarise(Nst = sum(status)) 
        
        # number of dead individuals in the second census
        mort.n.c2 <-  mort %>% group_by(plotcode) %>%
          mutate(status = ifelse(status == 0, 1, 0)) %>% # in order to sum the values, ifelse will change dead(0) to dead(1) *just in this calculation* no changes applicable to the rest of the data
          summarise(mortality = sum(status)) 
        
        # number of recruited individuals in the second census 
        rec.n.c2 <- rec %>% group_by(plotcode) %>%
          mutate(status = as.numeric(status)) %>% 
          summarise(recruits = sum(status)) 
        
        # initial number of individuals (N0)
        initial.number <- surv %>% filter(census.n == 1) %>% 
          bind_rows(mort) %>% 
          mutate(status = ifelse(status == 0, 1, 1), # in order to sum the values, ifelse will change dead(0) to dead(1) *just in this calculation* no changes applicable to the rest of the data
                 status = as.numeric(status)) %>% 
          group_by(plotcode) %>%
          summarise(N0 = sum(status)) 
        
        # final number of individuals (Nt)
        final.number <- full_join(surv.n.c2,rec.n.c2) %>% 
          mutate(across(recruits, coalesce, 0)) %>% #in case there is zero recruitment
          mutate(Nt = (Nst)+(recruits))
        
        demographic_rates_per_plot <- data.frame(N0 = initial.number$N0,
                                                 Nt = final.number$Nt,
                                                 Nst = surv.n.c2$Nst,
                                                 t = time$time.interv,
                                                 plotcode = time$plotcode) %>% 
          dplyr::mutate(N.ind.inicial = N0,
                        N.ind.final = Nt,
                        ma = 1-((Nst/N0)^(1/t)),
                        raf = 1-((Nst/Nt)^(1/t)), 
                        turnover = (ma+raf)/2) %>% 
          relocate(plotcode, .before = N0)
        
        estimates <- demographic_rates_per_plot
      }
      
      # retun data ----
      return(estimates) },
    all.diversity = function (survival,mortality,recruitment,trait){
      
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
      diversity.density <- data.frame(plotcode = rownames(comm.c1.density),
                                      rich.dens.census1 = hill_taxa(comm = comm.c1.density, q = 0),
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
      
      #png('results/NMDS.census1.png', units="in", width=5, height=5, res=300)
      nmds.c1 <- ggplot(nmds.census1.scores, mapping = aes(x = MDS1, y = MDS2)) + geom_point()+
        geom_vline(xintercept=0, color="black", linetype="dotted") +
        geom_hline(yintercept=0, color="black", linetype="dotted") +
        theme_light()+
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position = "bottom",
              panel.background = element_blank())
      #dev.off()
      
      nmds_census2 <- metaMDS(comm.c2.density, distance = "bray", autotransform = FALSE)
      nmds_census2
      nmds.census2.scores <- as.data.frame(nmds_census1$points)
      
      #png('results/NMDS.census2.png', units="in", width=5, height=5, res=300)
      nmds.c2 <- ggplot(nmds.census2.scores, mapping = aes(x = MDS1, y = MDS2)) + geom_point()+
        geom_vline(xintercept=0, color="black", linetype="dotted") +
        geom_hline(yintercept=0, color="black", linetype="dotted") +
        theme_light()+
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position = "bottom",
              panel.background = element_blank())
      #dev.off()
      
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
      comm.c1.density <- comm.c1.density %>% 
        mutate(plotcode = rownames(comm.c1.density)) %>% 
        relocate(plotcode)
      comm.c2.density <- comm.c2.density %>% 
        mutate(plotcode = rownames(comm.c1.density)) %>% 
        relocate(plotcode)
      
      functionalcensus1 <- dbFD(x = trait1, a = comm1)
      print("functional metrics from census 1 successfully obtained")

      functionalcensus2 <- dbFD(x = trait2, a = comm2)
      print("functional metrics from census 2 successfully obtained")
      
      plotcodes = data.frame(plotcodes = diversity.density$plotcode)
      
      estimates <- list(taxonomic.diversity = diversity.density, 
                        plotcodes = plotcodes,
                        functional_census1 = functionalcensus1,
                        functional_census2 = functionalcensus2,
                        community_matrix_c1 = comm.c1.density,
                        community_matrix_c2 = comm.c2.density,
                        nmds.c1 = nmds.c1,
                        nmds.c2 = nmds.c2) 
      
      return(estimates)
    })
   
  # Roda o pipeline e guarda todos os resultados
  resultados_pipeline <- eventReactive(input$run, {
    # Simula o dataset original que deve estar disponível no ambiente Shiny
    # Aqui, você usaria o seu data.complete real.
    df <- dados_iniciais
    trt <- trait.data
    resultados <- list("Planilha original" = df)
    
    # aplica f0 (ontogeny)
    df <- minhas_funcoes$ontogeny(df, ontogeny = input$ontogeny)
    resultados[["ontogeny"]] <- as.data.frame(df)
    
    # aplica f1 (correct.zombie)
    df <- minhas_funcoes$correct.zombie(df)
    resultados[["correct.zombie"]] <- as.data.frame(df)
    
    # aplica f2 (correct.diameter) com parametro do input
    df <- minhas_funcoes$correct.diameter(df, 
                                          census.n = input$census.n,
                                          dbh = input$dbh,
                                          WD.info = input$WD.info,
                                          H.info = input$H.info)
    resultados[["correct.diameter"]] <- as.data.frame(df)
    
    
    # aplica f3 (carbon.est) com parametro do input
    df <- minhas_funcoes$carbon.est(df, 
                                    WD.info = input$WD.info,
                                    H.info = input$H.info,
                                    dbh = input$dbh)
    resultados[["carbon.est"]] <- as.data.frame(df)
    
    # aplica f4 (demography) com parametro do input
    demography_result <- minhas_funcoes$demography(df, 
                                                   metric = input$metric,
                                                   census.n = input$census.n)
    
    # CORREÇÃO: Salva os resultados da demography como dataframes individuais
    resultados[["demography.survival"]] <- as.data.frame(demography_result$survival)
    resultados[["demography.recruitment"]] <- as.data.frame(demography_result$recruitment)
    resultados[["demography.mortality"]] <- as.data.frame(demography_result$mortality) 
    
    # aplica f5 (unmatch.stems) com parametro do input
    
    if(input$metric == "carbon") {
      survival_data <- demography_result$survival
      unmatch_result <- minhas_funcoes$unmatch.stems(survival_data)
      resultados[["unmatch.stems"]] <- as.data.frame(unmatch_result)
      
      # ATUALIZA: usa o survival corrigido nas funções seguintes
      survival_corrigido <- unmatch_result
    } else {
      # Mantém o survival original quando for "vital"
      survival_corrigido <- demography_result$survival
      resultados[["unmatch.stems"]] <- data.frame(Nota = "Funcao unmatch.stems so funciona quando metric = 'carbon'")
    }
    
    # aplica f6 (demography and forest dynamics) com parametro do input
    df.dyn <- minhas_funcoes$demography.and.dynamics(
      survival = survival_corrigido,  # USA O SURVIVAL CORRIGIDO QUANDO metric=carbon
      mortality = demography_result$mortality, 
      recruitment = demography_result$recruitment,
      metric = input$metric
    )
    resultados[["forest.dynamics"]] <- as.data.frame(df.dyn)
    # aplica f7 (all.diversity) com parametro do input
    all.div <- minhas_funcoes$all.diversity(
      survival = survival_corrigido,  # USA O SURVIVAL CORRIGIDO QUANDO metric=carbon
      mortality = demography_result$mortality, 
      recruitment = demography_result$recruitment,
      trait = trt
    )
    resultados[["taxonomic.diversity"]] <- as.data.frame(all.div$taxonomic.diversity)
    resultados[["functional_census1"]] <- as.data.frame(all.div$functional_census1)
    resultados[["functional_census2"]] <- as.data.frame(all.div$functional_census1)
    resultados[["comm.c1.density"]] <- as.data.frame(all.div$community_matrix_c1)
    resultados[["comm.c2.density"]] <- as.data.frame(all.div$community_matrix_c2)
    resultados[["plotcodes"]] <- as.data.frame(all.div$plotcodes)

    
    return(resultados)
  })
  
  # Cria abas com tabela + download individual para cada passo
  output$resultados <- renderUI({
    req(resultados_pipeline())
    res <- resultados_pipeline()
    
    tabs <- lapply(names(res), function(nome) {
      if(grepl("plot", nome)) {
        # Se for um plot, renderizar como plotOutput
        tabPanel(
          nome,
          plotOutput(paste0("plt_", gsub("[^a-zA-Z0-9]", "_", nome))))} 
      else {
        # Se for dados, renderizar como tableOutput
        tabPanel(
          nome,
          downloadButton(paste0("download_", gsub("[^a-zA-Z0-9]", "_", nome)), "Baixar .csv"),
          br(), br(),
          tableOutput(paste0("tbl_", gsub("[^a-zA-Z0-9]", "_", nome)))
        )}})
    
    do.call(tabsetPanel, tabs)})    

  
  # Renderiza tabelas e botões de download individual
  observeEvent(resultados_pipeline(), {
    res <- resultados_pipeline()
    
    lapply(names(res), function(nome) {
      local({
        nm <- nome
        safe_nm <- gsub("[^a-zA-Z0-9]", "_", nm)
        
        # Renderiza tabela (APENAS para dados)
        output[[paste0("tbl_", safe_nm)]] <- renderTable({
          if(is.data.frame(res[[nm]])) {
            head(res[[nm]], 20)
          } else {
            # Se não for dataframe, mostra mensagem
            data.frame(Nota = "Estes são resultados não-tabulares")}})
        
        # Renderiza download (SEMPRE cria o botão)
        output[[paste0("download_", safe_nm)]] <- downloadHandler(
          filename = function() {
            paste0(safe_nm, ".csv")
          },
          content = function(file) {
            if(is.data.frame(res[[nm]])) {
              write.csv(res[[nm]], file, row.names = FALSE)
            } else {
              # Se não for dataframe, cria um CSV com mensagem
              write.csv(data.frame(Nota = "Estes são resultados não-tabulares"), file, row.names = FALSE)
            }})})})
  })
  # dentro do downloadHandler, na parte final
  output$resultados <- renderUI({
    req(resultados_pipeline())
    res <- resultados_pipeline()
    
    tabs <- lapply(names(res), function(nome) {
      tabPanel(
        nome,
        downloadButton(paste0("download_", gsub("[^a-zA-Z0-9]", "_", nome)), "Baixar CSV"),
        br(), br(),
        tableOutput(paste0("tbl_", gsub("[^a-zA-Z0-9]", "_", nome)))
      )
    })
    
    do.call(tabsetPanel, tabs)
  })
  output$download_zip <- downloadHandler(
    filename = function() {
      "result.for.str.dyn.zip"
    },
    content = function(file) {
      res <- resultados_pipeline()
      tmpdir <- tempdir()
      files <- c()
      
      # cria CSV de todos os resultados, convertendo se necessário
      for (nm in names(res)) {
        fpath <- file.path(tmpdir, paste0(gsub("[^a-zA-Z0-9]", "_", nm), ".csv"))
        
        if(is.data.frame(res[[nm]])) {
          write.csv(res[[nm]], fpath, row.names = FALSE)
        } else {
          # Converte objetos não-dataframe para dataframe
          df_convertido <- tryCatch({
            as.data.frame(res[[nm]])
          }, error = function(e) {
            data.frame(Resultado = paste("Objeto não convertível:", nm))
          })
          write.csv(df_convertido, fpath, row.names = FALSE)
        }
        files <- c(files, fpath)
      }
      
      # cria arquivo com as funções
      funcoes_path <- file.path(tmpdir, "funcoes_pipeline.R")
      sink(funcoes_path)
      for (nome in names(minhas_funcoes)) {
        cat(paste0(nome, " <- ", deparse(minhas_funcoes[[nome]]), "\n\n"))
      }
      sink()
      files <- c(files, funcoes_path)
      
      # normaliza caminhos (necessário no Windows)
      files <- normalizePath(files)
      
      # cria ZIP com todos os arquivos
      zip::zipr(file, files)
    }
  )
}

shinyApp(ui, server)
