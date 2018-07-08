### ### ### ### ### ### ### ### ### ### ### ### ### 
#  CLASSIFYING MIGRATORY BEHAVIOR OF INDIVIDUALS  #
#         USING NET SQUARED DISPLACEMENT          #
#                                                 #
#         Kristin Barker | Summer 2018            #
#           kristinjbarker@gmail.com              #
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
      "tibble",
      "tidyr",
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
        dbq=C:/Users/kristin/Documents/MontanaElkCode/mtElkCode/Statewide_Elk_GPS.accdb")
  
     
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
          # format Date as date
          Date = as.Date(Date),          
          # identify year and month of each location
          yearLoc = substr(Date, 0, 4),
          monthLoc = substr(Date, 6, 7),
          # identify elk-year
          elkYr = paste(AnimalID, yearLoc, sep = "-"),
          # combine date and time in POSIXct format
          DT = as.POSIXct(paste(Date, Time, sep = " "), format = "%Y-%m-%d %H:%M:%S")
        ) 

      
      
      # fix NAs created by locs recorded during daylight savings time
      locsFormat$DT <- ifelse(!is.na(locsFormat$DT), locsFormat$DT,
        as.POSIXct(paste(locsFormat$Date,
          strftime((as.POSIXct(locsFormat$Time, format = "%H:%M:%S") - 1*60*60), format = "%H:%M:%S"),
          sep = " "), format = "%Y-%m-%d %H:%M:%S"))
      locsFormat$DT = as.POSIXct(locsFormat$DT, origin = '1970-01-01')

      

      # subset individuals and locations to be used in analysis
      locsModel <- locsFormat %>%
        # remove NA locations
        filter(!is.na(Latitude)) %>%        
        # only use indivs with at least 9 mos of locs
        group_by(elkYr) %>%
        filter(length(unique(monthLoc)) > 8) %>%
        ungroup() %>%
        # classify times as night or day (day is between 8am and 8pm, inclusive)
        mutate(
          Hour = as.numeric(substr(Time, 1, 2)),
          timeOfDay = ifelse(Hour >= 8 & Hour <= 20, "Day", "Night")) %>%
        # randomly select 1 day and 1 night loc per date (so, 2 locs per 24-hour period) per indiv
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
      
      
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    #### ~~~~ KRISTIN DELETE THIS WHEN DB GETS FINALIZED ~~~~ ####
            # it's the elk with the screwy locations #
      
      locsModel <- filter(locsModel, elkYr != "BROOT0015-2011")
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #       
      
      
      
      
   
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
  
    
      
     #### Make sure no indivs will throw errors due to rloc (a common issue)
    
       # redefine kappa parameter to help avoid issues
        uk64 <- pEst(u.k = log(64))
        
        # create dataframe to store error messages in
        errors <- data.frame(elkYr = unique(locsModel$elkYr), Err = NA)
        
        # for each individual
        for(i in 1:nrow(rlocs)) {
          
          # subset its locations
          ilocs <- droplevels(semi_join(locsModel, rlocs[i,], by = c("elkYr" = "burst")))
          
          # make it ltraj
          ilt <- as.ltraj(xy = ilocs[,c("utmX", "utmY")], date = ilocs$DT, id = ilocs$elkYr)
          
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
        
        # If no errors, consider yourself lucky and move on to the next section.
        # If errors, run KRISTIN INSERT EITHER PACKAGE OR FILE NAME HERE to assess.
        # Sometimes you can just rerun above code from "locsModel <- ..." to fix,  
        # if issues are just due to the locations that happened to be randomly selected.

                


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #     
    
    
### ### ### ### ### ###
####    |MODELS|   ####
### ### ### ### ### ###     

       
    
    #### Define initial parameter constraints ####
        
        # expand default duration on summer range to allow up to 8 months (default was 84 days)
        timing <- pEst(u.r = 240)  

      
    
    #### Define base model ####
        
        
        # rNSD with expanded duration parameter and updated rloc 
        mBase <- mvmtClass(lt, rloc = rlocs$rloc, p.est = timing) 
        
        # print number of convergence issues
        length(which(!fullmvmt(mBase))) # 65 (2018-07-03)
    
    
      
    #### Refine base model to address as many convergence issues as possible ####
        
          
        # allow up to 8km daily displacement within the same resident range
        uk64 <- pEst(u.k = log(64))
        
        # run updated model
        mref1 <- refine(mBase, p.est = uk64)
        
        # print number of convergence issues
        length(which(!fullmvmt(mref1))) # 30 (2018-07-03)


      
    #### Identify top model for each individual ####
      
        # don't consider mixmig or nomad (see thesis ch2 appendix s1 for more info)
        mTop <- topmvmt(mref1, omit = c("mixmig", "nomad"))
        
        # identify prelimiary top model per indiv
        topMods <- data.frame(elkYr = elkYrs, PrelimClassn = names(mTop))
        
        # store
        write.csv(topMods, file = "prelimBehavClassns.csv", row.names=F)
          
        # take quick look at prelim classns 
        summary(topMods$PrelimClassn)


	  

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #     
    
    
### ### ### ### ### ###
####  |NSD PLOTS|  ####
### ### ### ### ### ###     


      
      #### Define number of plots to create ####
          
        numPlots <- nrow(elkYrs)
        myPlots <- vector(numPlots, mode='list')
      
      
      
      ##### Create a plot for each individual ####
      
        for(i in 1:numPlots) {
          plot(mref1[[i]])
          myPlots[[i]] <- recordPlot()
        }
        graphics.off()
      
      
      
      #### Create and store one pdf file of all plots ####
        
        pdf('NSDplots.pdf', onefile = TRUE)
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
          params <- data.frame(elkYr = as.character(), 
                             Model = as.character(),
                             delta = as.numeric(),
                             phi = as.numeric(),
                             theta = as.numeric(),
                             rho = as.numeric(),
                             phi2 = as.numeric(),
                             kappa = as.numeric(),
                             gamma = as.numeric())
          
          
          # extract and store parameters of top models
          for (i in 1:length(paramlist)){
            # extract data for each model
            dat <- paramlist[[i]]
            # map parameters to correct individuals
            dat$elkYr <- row.names(dat)
            # identify model 
            dat$Model <- paste(names(paramlist)[i])
            # store all info
            params <- full_join(params, dat)
          }

      
      
      ## implement rules to reclassify some behaviors based on parameters ##
      
          
          reclass <- params %>%
            # if "resident" moved >900km2, reclassify as Migrant (these are actually mixed migrants)
            mutate(Reclass = ifelse(Model == "resident" & gamma > 900, "migrant", Model)) %>%
            # if animal "dispersed" or "migrated" <6.7 km, reclassify as Resident
            mutate(Reclass = ifelse(
              Reclass == "disperser" &  delta < 45 | Reclass == "migrant" &  !is.na(delta) & delta < 45, "resident", Reclass)) %>%
            # if animal "migrated" < 8.7km or "resided" within > 6.7km, reclassify as Other
            mutate(Reclass = ifelse(Reclass == "migrant" & !is.na(delta) & delta < 75, "other", Reclass)) %>%
            mutate(Reclass = ifelse(Reclass == "resident" & !is.na(gamma) & gamma > 45, "other", Reclass)) %>%  
            # rename dispersers as Other
            mutate(Reclass = ifelse(Reclass == "disperser", "other", Reclass)) %>%
            # if animal "dispersed" or "migrated" after summer, reclassify as Other
            mutate(Reclass = ifelse(!is.na(theta) & theta > 270, "other", Reclass)) 



  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #     
  
          
    
    
### ### ### ### ### ### ### ### ###
####    |BEHAVIOR SUMMARIES|   ####
### ### ### ### ### ### ### ### ###
	  
         

        #### Final behavioral classifications, including Herd info ####
          
        behav <- reclass %>%
          left_join(elkYrHerd, by = "elkYr") %>%
          rename(Behav = Reclass, ParamMod = Model) %>%
          dplyr::select(c(elkYr, Herd, Behav, delta, phi, theta, rho, phi2, kappa, ParamMod)) %>%
          arrange(elkYr)
        write.csv(behav, "behavParamsPerIndiv.csv", row.names = F)
        
        
        
        #### Herd-level summaries - Ppns of behavs; mean parameters ####
        
        herdBehav <- behav %>%
    	    group_by(Herd) %>%
    	    summarise(nIndivs = n(),
    	              nMig = length(which(Behav == "migrant")),
    	              nRes = length(which(Behav == "resident")),
    	              nOth = length(which(Behav == "other"))) %>%
    	    mutate(ppnMig = round(nMig/nIndivs, digits = 2),
    	           ppnRes = round(nRes/nIndivs, digits =2),
    	           ppnOth = round(nOth/nIndivs, digits = 2)) %>%
    	    dplyr::select(Herd, ppnMig, ppnRes, ppnOth, nIndivs)
        write.csv(herdBehav, "behavPerHerd.csv", row.names=F)
        
        
        
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #     
  
          
    
    
### ### ### ### ### ### ### ### ###
####    |SEASONAL SUMMARIES|   ####
### ### ### ### ### ### ### ### ###
	  

        
        
    #### Determine spring and fall movement dates for migrants  ####
        
        
        # Identify mixed migrants (need to handle specially)
        indivsMix <- reclass %>%
          filter(Reclass == "migrant" & gamma > 900) %>%
          dplyr::select(elkYr)
        indexMix <- which(elkYrs$elkYr %in% indivsMix$elkYr)
        
        
        # Identify migrants
        indivsMig <- reclass %>%
          filter(Reclass == "migrant") %>%
          # but not mixed migrants bc they don't always have a migrant model
          anti_join(indivsMix, by = "elkYr") %>%
          dplyr::select(elkYr)  
        indexMig <- which(elkYrs$elkYr %in% indivsMig$elkYr)

        
        # extract movement dates from migrant models
        datesModelA <- list()
        for (i in 1:length(indexMig)) {
          datesModelA[i] <- mvmt2dt(mref1[[indexMig[i]]])
          names(datesModelA)[[i]] <- elkYrs[indexMig[i], "elkYr"]
        }
        
        # extract movement dates from mixed-migrant models
        datesModelB <- list()
        for (i in 1:length(indexMix)) {
          datesModelB[i] <- mvmt2dt(mref1[[indexMix[i]]], mod = "mixmig")
          names(datesModelB)[[i]] <- elkYrs[indexMix[i], "elkYr"]
        }
        
        # combine dates to incl migrants and mixed-migrants
        datesModels <- c(datesModelA, datesModelB)
        
        
        
        # Create dataframe to store properly formatted date data
        datesMig <- as.data.frame(matrix(ncol = 5, nrow = 0))
        colnames(datesMig) <- c("elkYr", "end1", "end2", "str1", "str2")
        
        
        # Format & store movement dates for all migrants & mixed migrants
        for (i in 1:length(datesModels)) {
          
          # For each individual
          dat <- datesModels[[i]]
          
          # Add column of parameter names
          dat$param <- rownames(dat)
          
          # Add column of individual identifier
          dat$elkYr <- names(datesModels[i])
          
          # Remove extraneous column that messes things up
          dat <- dplyr::select(dat, -dday)
          
          # Format data appropriately to combine with other indivs
          dat <- spread(dat, key = "param", value = "date")

          # And combine
          datesMig <- bind_rows(datesMig, dat)
          
        }
           
        
        # Calculate movement durations, seasonal dates, and distance moved
        datesIndiv <- datesMig %>%
          # pull spring and fall migration dates from the above
          mutate(
            sprSt = as.Date(str1),
            sprEn = as.Date(end1),
            fallSt = as.Date(str2),
            fallEn = as.Date(end2),
            smrSt = sprEn+1,
            smrEn = fallSt-1) %>%
          # calculate duration of spring and fall migration
          mutate(
            sprDr = as.numeric(round(sprEn - sprSt)),
            fallDr = as.numeric(round(fallEn - fallSt))) %>%
          # remove extraneous columns
          dplyr::select(-c("str1", "end1", "str2", "end2")) %>%
          # pull in model parameters for summer range distance and duration
          left_join(dplyr::select(params, elkYr, delta, rho), by = "elkYr") %>%
          rename(dist = delta, smrDr = rho) %>%
          mutate(dist = as.numeric(round(dist, 2)), smrDr = as.numeric(round(smrDr))) %>%
          # add herd and herd-year to facilitate later herd summaries etc
          left_join(elkYrHerd, by = "elkYr") %>%
          mutate(
            yr = substr(elkYr, nchar(elkYr)-3, nchar(elkYr)),
            herdYr = paste(Herd, yr, sep = "-"))

        
        # Summarize the above per herd        
        datesHerd <- datesIndiv %>%
          group_by(herdYr) %>%
          summarise(
            nMig = n(),
            sprStartMean = mean(sprSt),
            sprStartSd = as.numeric(sd(sprSt)),
            sprEndMean = mean(sprEn),
            sprEndSd = as.numeric(sd(sprEn)),
            sprDurMean = mean(sprDr),
            sprDurSd = sd(sprDr),
            fallStartMean = mean(fallSt),
            fallStartSd = as.numeric(sd(fallSt)),
            fallEndMean = mean(fallEn),
            fallEndSd = as.numeric(sd(fallEn)),
            fallDurMean = mean(fallDr),
            fallDurSd = sd(fallDr),
            smrDurMean = mean(smrDr),
            smrDurSd = sd(smrDr),
            distMean = mean(dist),
            distSd = sd(dist))
        
        
        # Add all date and duration info to full indiv list
        allDatIndiv <- reclass %>%
          rename(behav = Reclass) %>%
          left_join(elkYrHerd, by = "elkYr") %>%
          mutate(herdYr = paste(Herd, substr(elkYr, nchar(elkYr)-3, nchar(elkYr)), sep = "-")) %>%
          left_join(datesHerd, by = "herdYr") %>%
          left_join(datesIndiv, by = "elkYr") %>%
          dplyr::select(-c(Herd.y, herdYr.y)) %>%
          rename(Herd = Herd.x, herdYr = herdYr.x) %>%
          # set dates per indiv based on either their migration or on average herd date
          mutate(
            sprStart = as.Date(ifelse(behav == "migrant", sprSt, sprStartMean), origin = '1970-01-01'),
            sprEnd = as.Date(ifelse(behav == "migrant", sprEn, sprEndMean), origin = '1970-01-01'),
            sprDur = ifelse(behav == "migrant", sprDr, sprDurMean),
            fallStart = as.Date(ifelse(behav == "migrant", fallSt, fallStartMean), origin = '1970-01-01'),
            fallEnd = as.Date(ifelse(behav == "migrant", fallEn, fallEndMean), origin = '1970-01-01'),
            fallDur = ifelse(behav == "migrant", fallDr, fallDurMean),
            smrDur = ifelse(behav == "migrant", smrDr, smrDurMean),
            dist = ifelse(behav == "migrant", dist, NA)) %>%   
          dplyr::select(elkYr, Herd, herdYr, behav, 
            sprStart, sprEnd, sprDur, fallStart, fallEnd, fallDur, smrDur, dist) 
            
        
        
        # Define winter for each individual
        winIndiv <- locsModel %>%
          # start winter at first recorded location
          group_by(elkYr) %>%
          summarise(winStart = first(DT)) %>%
          droplevels() %>%
          # end winter day before spring migration started          
          left_join(allDatIndiv, by = "elkYr") %>%
          mutate(winEnd = (sprStart - 1))
          

        
        
        
        
        
## ## ## ## ## ~ OLDER CODE ## ## ## ## ## ## ## ## ## ## ## ## ## ##
                  
          # use migration dates to define winter
          indivDate <- locsModel %>%
            distinct(elkYr, Day1) %>%
            left_join(reclass, by = "elkYr") %>%
            filter(Reclass == "migrant") %>%
            left_join(rlocs, by = c("elkYr" = "burst")) %>%
            mutate(stdt =  Day1+newrloc) %>%
            mutate(migDate = stdt+theta) %>%
            mutate(MigDay = substr(migDate, 6, 10)) %>%
            dplyr::select(elkYr, stdt, migDate, MigDay) %>%
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

                                