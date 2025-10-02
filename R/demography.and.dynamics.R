############################################################
########  DEMOGRAPHIC RATES AND FOREST DYNAMICS  ###########
############################################################
### Code updated by Kauane M Bordin at 02 Oct 2025

### Description: demographic rates (recruitment, mortality, turnover, individual-based)
# and forest dynamics (carbon productivity and losses, stem-based) 

### In this code we calculate demographic rates and forest dynamics. 
# Demographic rates are based on equations from Kohyama et al 2018 MEE and
# Phillips et al 2008 Science
# Demographic rates require the dataset os survivors, recruits, and deads, metric='vital' derived from demography function
# Forest dynamics are based on equations from Kohyama et al 2019 FEM
# Forest dynamics requires the dataset os survivors, recruits, and deads, metric='carbon' derived from demography function

demography.and.dynamics <- function (survival,mortality,recruitment,metric){
  #' @description generate data for demographic rates and forest dynamics
  #' @author Kauane Maiara Bordin
  #' @param  data frame with stem (carbon) or individual (vital) level data across censuses
  #' @return data frame with demographic rates (recruitment, mortality, turnover) or
  #'         forest dynamics (productivity and losses)
  #' survival: derived from demography function
  #' mortality: : derived from demography function
  #' recruitment: : derived from demography function
  #' metric: "vital" or "carbon"
  
  # calculate stem/individual basal area
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
    # AGC and basal area of dead stems in census2
    # it is based on stem AGC in census 1, when they were alive
    mort  <-  mort %>% 
      mutate(mort.agc.total.ha = AGC/plot.area, # stem AGCmort per hectare
             mort.agc.total.ha.yr = mort.agc.total.ha/time.interv, # stem AGCmort per hectare per year
             status = as.numeric(status), # might be 0
             ba = basal.area(d)) # calculate basal area
    
    # plot-level AGC and basal area of dead stems in census2
    # it is based on AGC in census 1, when the stems were alive
    mort.agc.total.ha <- mort %>% 
      group_by(plotcode) %>%
      summarise(mort.agc.total.ha = sum(mort.agc.total.ha),
                ba.mort = sum(ba))  
    
    # summarising recruitment data ------
    # AGC and basal area of recruited stems in census2
    # it is based on stem AGC in census 2, when they were alive
    rec <- rec %>% 
      mutate(rec.agc.total.ha = AGC/plot.area, # stem AGCrec per hectare
             rec.agc.total.ha.yr = rec.agc.total.ha/time.interv, # stem AGCrec per hectare per year
             status = as.numeric(status), #might be 1
             ba = basal.area(d)) # calculate basal area
    
    # plot-level AGC and basal area per ha and per year of recruited stems in census2
    # it is based on AGC in census 2, when the stems were alive
    rec.agc_gain.ha.yr = rec %>% 
      group_by(plotcode) %>%
      summarise(rec.ha.yr = sum(rec.agc.total.ha.yr),
                ba.rec = sum(ba)) 
    
    # plot-level AGC and basal area of recruited stems in census2
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
                ba.surv.t2 = sum(ba.surv.t2)) 
    
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
    carbon_estimates_per_plot <- data.frame(Bs0 = surv.agc$agc.total.surv.ha.c1,
                                            B0 = all.carbon.c1$B0,
                                            Bt = all.carbon.c2$Bt,
                                            t = time$time.interv,
                                            plotcode = time$plotcode,
                                            gains = all.carbon.gains.c2$gains, # surv growth + rec
                                            incr.surv = surv.agc$surv.agc_gain.ha.yr, # surv growth
                                            basal.area.total$basal.area_c1,
                                            basal.area.total$basal.area_c2) %>% 
      left_join(mort.agc.total.ha) %>% 
      left_join(rec.agc_gain.ha.yr) %>%
      dplyr::rename(losses = mort.agc.total.ha,
                    recruit.incr.ha.yr = rec.ha.yr) %>% 
      relocate(plotcode, .before = Bs0) %>% 
      mutate(across(where(is.numeric), tidyr::replace_na, 0)) %>% #in case there is na
      dplyr::mutate(AGC.ha.census1 = B0,
                    AGC.ha.census2 = Bt,
                    Pann = ((1-((Bs0/Bt)^(1/t)))*(Bt-B0))/((t*(1-(B0/Bt)^(1/t)))), # Kohyama  eq 3
                    Lann = ((1-((Bs0/B0)^(1/t)))*(Bt-B0))/((t*(((Bt/B0)^(1/t))-1))), # Kohyama eq 4
                    net.carbon.change = Pann-Lann) %>% 
      mutate(across(losses, coalesce, 0)) %>% #in case this is zero
      mutate(across(recruit.incr.ha.yr, coalesce, 0)) %>% #in case this is zero
      mutate(across(everything(), ~ ifelse(is.nan(.), 0, .)))
    
    estimates <- carbon_estimates_per_plot %>% 
      left_join(gps, by="plotcode")
    
  }
  #-------------------------------------------------------------------------------------
  if(metric == "vital"){
    print("METRIC == VITAL RATES")
    
    #time census interval (t)
    time <- surv %>% ungroup() %>% 
      dplyr::select(plotcode,time.interv) %>% 
      unique() %>% arrange(plotcode)
    
    #number of survival individuals in the second census (Nst)
    surv.n.c2 <- surv %>% 
      filter(census.n == 2)%>% 
      group_by(plotcode) %>%
      mutate(status = as.numeric(status)) %>% 
      summarise(survivors = sum(status)) 
    
    #number of dead individuals in the second census
    mort.n.c2 <-  mort %>% group_by(plotcode) %>%
      mutate(status = ifelse(status == 0, 1, 0)) %>% #in order to sum the values, ifelse will change dead(0) to dead(1) *just in this calculation* no changes applicable to the rest of the data
      summarise(mortality = sum(status)) 
    
    #number of recruited individuals in the second census 
    rec.n.c2 <- rec %>% group_by(plotcode) %>%
      mutate(status = as.numeric(status)) %>% 
      summarise(recruits = sum(status)) 
    
    #initial number of individuals (N0)
    initial.number <- surv %>% filter(census.n == 1) %>% 
      bind_rows(mort) %>% 
      mutate(status = ifelse(status == 0, 1, 1),#in order to sum the values, ifelse will change dead(0) to dead(1) *just in this calculation* no changes applicable to the rest of the data
             status = as.numeric(status)) %>% 
      group_by(plotcode) %>%
      summarise(N0 = sum(status)) 
    
    #final number of individuals (Nt)
    final.number <- full_join(surv.n.c2,rec.n.c2) %>% 
      mutate(across(recruits, coalesce, 0)) %>% #in case there is zero recruitment
      mutate(Nt = (survivors)+(recruits))
    
    #Nst
    surv.n.c2
    
    demographic_rates_per_plot <- data.frame(N0 = initial.number$N0,
                                             Nt = final.number$Nt,
                                             Nst = surv.n.c2$survivors,
                                             t = time$time.interv,
                                             plotcode = time$plotcode) %>% 
      dplyr::mutate(N.stem.inicial = N0,
                    N.stem.final = Nt,
                    ma = 1-((Nst/N0)^(1/t)),
                    raf = 1-((Nst/Nt)^(1/t))) %>% 
      relocate(plotcode, .before = N0)
    
   
    
    estimates <- left_join(demographic_rates_per_plot, by="plotcode")
  }
  
  # retun data ----
  return(estimates)
  
}
