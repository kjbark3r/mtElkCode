### ### ### ### ### ### ### ### ### ### ### ### ### 
#  CLASSIFYING MIGRATORY BEHAVIOR OF INDIVIDUALS  #
#         USING NET SQUARED DISPLACEMENT          #
#                                                 #
#         Kristin Barker - Summer 2018            #
#            kristinjbarker@gmail.com             #
### ### ### ### ### ### ### ### ### ### ### ### ###



### ### ### ### ###
####  |SETUP|  ####
### ### ### ### ###



  #### Install packages ####


    # list of packages needed
    packages <- c(
      "RODBC",       ## connect to Access database
      "migrateR",    ## migratory behavior analysis
      "sp",          ## spatial work, eg setting projections
      "dplyr")       ## data manipulation and general awesomeness


    # have to install migrateR package specially (take it up with spitz)
    if (!"migrateR" %in% installed.packages()[, "Package"]) {
      devtools::install_github("dbspitz/migrateR/migrateR", build_vignettes = T) }
    
    
    # load all packages listed above, but first install any you don't already have
    if (!"devtools" %in% installed.packages()[, "Package"]) {install.packages("devtools")}
    devtools::install_github("kjbark3r/pkgTest")
    pkgTest::ipak(packages) 
    rm(packages)
    
    
  

  #### Define spatial projections ####

    
    # find more epsg codes @ http://spatialreference.org/ (search bar topR)
    latlong <- CRS("+init=epsg:4326") # Lat/Longs in WGS84 (typical for elk GPS collars)
    utm <- CRS("+init=epsg:3742") # NAD83(HARN)/UTMzone12N (required for migrateR)
    stateplane <- CRS("+init=epsg:2818") # here's Montana stateplane if you need it

    

  
        
### ### ### ### ### ### ### ### ###
####  |READ IN AND PREP DATA|  ####
### ### ### ### ### ### ### ### ###     
    
    
    
    #### Define your spatial projection ####    
      
      # replace "latlong" if needed
      proj <- latlong  
    
    
    
    #### Connect to Access database ####
    
    
        ## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
        ##                   -- IMPORTANT NOTE ABOUT THE BELOW LINE --                        ##
        ##  To work, you must be running the same versions of Access and R (64bit or 32bit).  ##
        ##     If they don't match, change your R version to match your Access version.       ## 
        ## To check Access version: Access > File > Account > About Access > 1st line at top  ##
        ##   To check/change R version: R > Tools > Global Options > General > R Version      ## 
        ## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    
    
      # replace the file path after "dbq=" with yours (sorry, i failed to do it programmatically)
      channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
        dbq = C:/Users/kristin/Documents/MontanaElkCode/mtElkCode/Statewide_Elk_GPS_copy.accdb")
  
     
      # pull GPS collar locations from database (this takes forEVer)
      locsRaw <- sqlQuery(channel, paste("select * from ElkGPS"))
      
      
      # close database connection
      odbcCloseAll()
  

      
    #### Prep data for analysis ####      

      
      # format collar data
      locsFormat <- locsRaw %>%
        # correctly classify each column
        mutate(
          # remove messed-up date from Time 
          Time = strftime(Time, format = "%H:%M:%S"),
          # combine date and time in POSIXct format
          DT = as.POSIXct(paste(Date, Time, sep = " "), format = "%Y-%m-%d %H:%M:%S"),
          # format Date as date
          Date = as.Date(Date),          
          # identify year and month of each location
          yearLoc = substr(Date, 0, 4),
          monthLoc = substr(Date, 6, 7),
          # identify elk-year
          elkYr = paste(AnimalID, yearLoc, sep = "-")
        ) 


      
      # identify individuals and locations to be used in analysis
      locsModel <- locsFormat %>%
        # remove NA locations
        filter(!is.na(Latitude)) %>%        
        # only use indivs with at least 9 mos of locs
        group_by(elkYr) %>%
        filter(length(unique(monthLoc)) > 8) %>%
        ungroup() %>%
        # identify time of day (day is between 8am and 8pm, inclusive)
        mutate(timeOfDay = ifelse(substr(Time, 1, 2) >= 8 & substr(Time, 1, 2) <= 20, "Day", "Night")) %>%
        # randomly select one loc per time of day (so, 2 locs per 24-hour period) per indiv
        group_by(elkYr, Date, timeOfDay) %>%
        sample_n(1) %>%
        ungroup() %>%
        # identify 1st date of data
        group_by(elkYr) %>%
        mutate(Day1 = min(Date)) %>%
        ungroup() %>%
        # only include 1st full yr of data, plus extra month for full return to winter
        filter(Date <= Day1 + 395) %>% 
        # remove indivs who didn't have opportunity to return to winter range (locs end b4 dec)
        group_by(elkYr) %>%
        filter(max(monthLoc) == 12) %>%
        ungroup() %>%
        # return to data frame (grouping made it a list)
        as.data.frame() %>%
        # remove pesky stored factor levels
        droplevels()
      
      # convert locations to UTMs (requirement of migrateR)
      locsModel <- as.data.frame(spTransform(SpatialPointsDataFrame(
        data.frame("utmX" = locsModel$Longitude, "utmY" = locsModel$Latitude), 
        locsModel, proj4string = latlong), utm))    
      
   
      # identify unique herds and indivs 
      elkYrHerd <- locsModel %>%
        dplyr::select(elkYr, Herd) %>%
        distinct()
      
      # KRISTIN remove these if unimportant
      elkYrs <- unique(dplyr::select(locsModel, elkYr)) 
      herds <- unique(dplyr::select(locsModel, Herd))   

  
      
    #### Format data as ltraj object ### 
      
      lt <- as.ltraj(
        xy = locsModel[,c("utmX", "utmY")], 
        date = locsModel$DT,
        id = locsModel$elkYr
        )    
      
            

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #     
    
    
### ### ### ### ### ### ### ### ### ### ### ### ##
####  |CHOOSE INDIVIDUAL STARTING LOCATIONS|  ####
### ### ### ### ### ### ### ### ### ### ### ### ##
    

    
   #### Identify most parsimonious starting location for NSD (rNSD) ####
      
    
    rlocs <- findrloc(lt) # see troubleshootingNSD.R if you get errors here   
  
    
      
     #### Find indivs whose models produce an error due to rloc; give them new rloc
    
       # redefine kappa parameter to help avoid issues
        uk64 <- pEst(u.k = log(64))
        
        # create dataframe to store error messages in
        errors <- data.frame(elkYr = unique(locsModel$elkYr), Err = NA)
        
        # for each individual
        for(i in 1:nrow(rlocs)) {
          
          # subset its locations
          ilocs <- droplevels(semi_join(locsModel, rlocs[i,], by = c("elkYr" = "burst")))
          
          # make it ltraj
          ilt <- as.ltraj(xy = ilocs[,c("utmX", "utmY")], date = ilocs$DT, id = ilocs$AnimalID)
          
          # try the model and store error message if any
          # (note: warning messages are generally ok here)
          tryCatch(mvmtClass(ilt, p.est = uk64, rloc = rlocs[i,"rloc"]), error = function(e) {
                  errors[i,"Err"] <<- conditionMessage(e)
                  NULL
              })
        }
        
        # individuals and the errors they had
        errors <- errors[!is.na(errors$Err),]
        View(errors)
                

#### ----KRISTIN YOU LEFT OFF HERE  ####
        
        ##, you have 3 errors. Next step is trying new rlocs to address.##
        
        
        # if any indivs had errors, run separate code to address
        if (any(!is.na(errors$Err))) {
          devtools::install_github("kjbark3r/rLocFix")
          rLocFix::changerLoc(data, whatever)
        }
        

        
        
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #     
    
    
### ### ### ### ### ###
####    |MODELS|   ####
### ### ### ### ### ###     

       
    
    #### Define initial parameter constraints ####
        
        # expand default duration on summer range to allow up to 8 months (default was 84 days)
        timing <- pEst(u.r = 240)  

      
    
    #### Define base model ####
        
        
        # rNSD with expanded duration parameter and updated rloc 
        mBase <- mvmtClass(lt, rloc = rlocs$newrloc, p.est = timing) 
        
        # print number of convergence issues
        length(which(!fullmvmt(mBase))) 
    
    
      
    #### Refine base model to address convergence issues ####
        
          
        # allow up to 8km daily displacement within the same resident range
        uk64 <- pEst(u.k = log(64))
        
        # run updated model
        mref1 <- refine(mBase, p.est = uk64)
        
        # print number of convergence issues
        length(which(!fullmvmt(mref1))) 


      
    #### Identify top model for each individual ####
      
        # don't consider mixmig or nomad (see thesis ch2 appx s1 for more info)
        mTop <- topmvmt(mref1, omit = c("mixmig", "nomad"))
        
        # identify prelimiary top model per indiv
        topMods <- data.frame(AnimalID = modIndivs, PrelimClassn = names(mTop))
        
        # store
        write.csv(topMods, file = "./rNSDresults/initialclassns.csv", row.names=F)
          
        # take quick look at prelim classns 
        summary(topMods$PrelimClassn)


	  

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #     
    
    
### ### ### ### ### ###
####  |NSD PLOTS|  ####
### ### ### ### ### ###     


      
      #### Define number of plots to create ####
          
        numPlots <- nrow(modIndivs)
        myPlots <- vector(numPlots, mode='list')
      
      
      
      ##### Create a plot for each individual ####
      
        for(i in 1:numPlots) {
          plot(mref1[[i]])
          myPlots[[i]] <- recordPlot()
        }
        graphics.off()
      
      
      
      #### Create and store one pdf file of all plots ####
        
        pdf(here(outputFolder, 'NSDplots.pdf'), onefile = TRUE)
        for(my.plot in myPlots) {
          replayPlot(my.plot)
        }
        graphics.off()
      
      
      
      
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #     
      
        
    
### ### ### ### ### ### ###
####    |PARAMETERS|   ####
### ### ### ### ### ### ###    

      
      
      
      #### Classify behaviors using parameter values ####
      
      
          # Create dataframe of parameters from each indiv model
          paramlist <- mvmt2df(mTop)
    
        
          # Create blank dataframe to store parameters of top models in
          params <- data.frame(AnimalID = as.character(), 
                             Model = as.character(),
                             delta = as.numeric(),
                             phi = as.numeric(),
                             theta = as.numeric(),
                             rho = as.numeric(),
                             phi2 = as.numeric(),
                             kappa = as.numeric())
          
          
          # extract and store parameters of top models
          for (i in 1:length(paramlist)){
            # extract data for each model
            dat <- paramlist[[i]]
            # map parameters to correct individuals
            dat$AnimalID <- row.names(dat)
            # identify model 
            dat$Model <- paste(names(paramlist)[i])
            # store all info
            params <- full_join(params, dat)
          }

      
      
      ## implement rules to reclassify some behaviors based on parameters ##
      
          
          reclass <- params %>%
            # if "resident" moved >900km2, reclassify as Migrant (these are actually mixed migrants)
            mutate(Reclass = ifelse(Model == "resident" & delta > 900, "migrant", Model)) %>%
            # if animal "dispersed" or "migrated" <6.7 km, reclassify as Resident
            mutate(Reclass = ifelse(Reclass == "disperser" &  delta < 45 | Reclass == "migrant" &  delta < 45, "resident", Reclass)) %>%
            # if animal "migrated" < 8.7km or "resided" within > 6.7km, reclassify as Other
            mutate(Reclass = ifelse(Reclass == "migrant" & delta < 75, "other", Reclass)) %>%
            mutate(Reclass = ifelse(Reclass == "resident" & delta > 45, "other", Reclass)) %>%  
            # rename dispersers as Other
            mutate(Reclass = ifelse(Reclass == "disperser", "other", Reclass)) %>%
            # if animal "dispersed" or "migrated" after summer, reclassify as Other
            mutate(Reclass = ifelse(!is.na(theta) & theta > 270, "other", Reclass)) 



  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #     
  
          
    
    
### ### ### ### ### ### ##
####    |SUMMARIES|   ####
### ### ### ### ### ### ##
	  
         
        ## add herd info, rename columns to avoid future confusion, and store ##
          
          
          # herd per indiv
          indivsHerds <- modLocs %>%
            dplyr::select(AnimalID, Herd) %>%
            distinct() 
          
          
          # add herd info to classification info
          behav <- reclass %>%
            left_join(indivsHerds, by = "AnimalID") %>%
            rename(Behav = Reclass, ParamMod = Model) %>%
            dplyr::select(c(AnimalID, Herd, Behav, delta, phi, theta, rho, phi2, kappa, ParamMod)) %>%
            arrange(AnimalID)
          write.csv(behav, "mig-behav.csv", row.names = F)
          
          
          # summarize behaviors by herd
          # without including the 2 indivs we can't estimate winterHR for
          popbehav <- behav %>%
            filter(AnimalID != "BROOT130031" & AnimalID != "BROOT130079") %>%
      	    group_by(Herd) %>%
      	    summarise(nIndivs = n(),
      	              nMig = length(which(Behav == "migrant")),
      	              nRes = length(which(Behav == "resident")),
      	              nOth = length(which(Behav == "other"))) %>%
      	    mutate(ppnMig = round(nMig/nIndivs, digits = 2),
      	           ppnRes = round(nRes/nIndivs, digits =2),
      	           ppnOth = round(nOth/nIndivs, digits = 2)) %>%
      	    dplyr::select(Herd, ppnMig, ppnRes, ppnOth, nIndivs)
          write.csv(popbehav, "behav-per-popn.csv", row.names=F)
          
          
          # use migration dates to define winter
          indivdate <- modLocs %>%
            distinct(AnimalID, Day1) %>%
            left_join(reclass, by = "AnimalID") %>%
            filter(Reclass == "migrant") %>%
            left_join(rlocs, by = c("AnimalID" = "burst")) %>%
            mutate(stdt =  Day1+newrloc) %>%
            mutate(migDate = stdt+theta) %>%
            mutate(MigDay = substr(migDate, 6, 10)) %>%
            dplyr::select(AnimalID, stdt, migDate, MigDay) %>%
            mutate(nDay = migDate-stdt) %>%
            left_join(indivsHerds) %>%
            filter(!is.na(migDate))
          capyrs <- dplyr::select(popnyrs, -nIndiv)
          capdat <- read.csv("popn-capdates.csv") %>% 
            semi_join(capyrs, by = c("Herd", "Year")) 
          herddate <- indivdate %>%
            group_by(Herd) %>%
            summarise(firstDate = min(migDate),
                      avgDate = mean(migDate)) %>%
            left_join(capdat) %>%
            dplyr::select(-nIndiv) %>%
            mutate(nDay = firstDate - as.Date(LastCapDate))

                                