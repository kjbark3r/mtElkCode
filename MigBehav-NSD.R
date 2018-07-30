### ### ### ### ### ### ### ### ### ### ### ### ### 
#  CLASSIFYING MIGRATORY BEHAVIOR OF INDIVIDUALS  #
#         USING NET SQUARED DISPLACEMENT          #
#  AND VOLUME OF INTERSECTION BT SEASONAL RANGES  #
#                                                 #
#         Kristin Barker | Summer 2018            #
#           kristinjbarker@gmail.com              #
### ### ### ### ### ### ### ### ### ### ### ### ###




### ### ### ### ###
####  |SETUP|  ####
### ### ### ### ###


  #### Identify working directory ####

    # set this to where you want to save your output files
    setwd("C:\\Users\\kristin\\Documents\\ElkMigrationAnalysis")



  #### Install packages ####


    # list of packages needed
    packages <- c(
      "RODBC",        ## connect to Access database
      "migrateR",     ## migratory behavior analysis
      "adehabitatHR", ## volume intersection calculation
      "sp",           ## spatial work, eg setting projections
      "rgdal",        ## create and store shapefiles
      "tibble",       ## apparently required for tidyr/dplyr now
      "tidyr",        ## data manipulation and general awesomeness
      "dplyr")        ## data manipulation and general awesomeness


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
    
    
    
    #### Define spatial projection ####    
      
      # replace "latlong" if needed
      proj <- latlong  
    
    
    
    #### Connect to Access database ####
    
    
        ## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
        ##                   -- IMPORTANT NOTE ABOUT THE BELOW LINE --                        ##
        ##    You must be running both Access and R in the same version (64bit or 32bit).     ##
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
        # remove the single elk from the Greycliff herd
        filter(Herd != "Greycliff") %>%
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
          DT = as.POSIXct(paste(Date, Time, sep = " "), format = "%Y-%m-%d %H:%M:%S")) %>%
        # remove NA locations
        filter(!is.na(Latitude))

      
      
      # fix NAs created by locs recorded during daylight savings time
      locsFormat$DT <- ifelse(!is.na(locsFormat$DT), locsFormat$DT,
        as.POSIXct(paste(locsFormat$Date,
          strftime((as.POSIXct(locsFormat$Time, format = "%H:%M:%S") - 1*60*60), format = "%H:%M:%S"),
          sep = " "), format = "%Y-%m-%d %H:%M:%S"))
      locsFormat$DT = as.POSIXct(locsFormat$DT, origin = '1970-01-01')

      

      # subset individuals and locations to be used in analysis
      locsModel <- locsFormat %>%
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
      
      
      
      
### ### ### ### ### ### ### ### ### ### 
####    |SUMMARIZE PREPPED DATA|   ####
### ### ### ### ### ### ### ### ### ###
      


      #### Elk-years, herds, and herd-years used in behavioral analysis ####
      elkYrHerd <- locsModel %>%
        dplyr::select(elkYr, Herd) %>%
        distinct() %>%
        mutate(herdYr =  paste(Herd, substr(elkYr, nchar(elkYr)-3, nchar(elkYr)), sep = "-"))  
      
      #### Just elk-years used in behavioral analysis ####
      elkYrs <- unique(dplyr::select(locsModel, elkYr)) 
      
      #### Just herds used in behavioral analysis ####
      herds <- unique(dplyr::select(locsModel, Herd))
      
      #### Elk-years, herds, and herd-years NOT used in behavioral analysis ####
      elkYrHerdNoBehav <- locsFormat %>%
        anti_join(elkYrs) %>%
        dplyr::select(elkYr, Herd) %>%
        distinct() %>%
        mutate(herdYr =  paste(Herd, substr(elkYr, nchar(elkYr)-3, nchar(elkYr)), sep = "-")) 
      
      #### All elk-years, herds, and herd-years ####
      elkYrHerdAll <- bind_rows(elkYrHerd, elkYrHerdNoBehav)    



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #     
    

    
### ### ### ### ### ###
####    |MODELS|   ####
### ### ### ### ### ###     

      
      
    #### Format data as ltraj object ####
      
      lt <- as.ltraj(
        xy = locsModel[,c("utmX", "utmY")], 
        date = locsModel$DT,
        id = locsModel$elkYr
        )  

      
      
    #### Identify most parsimonious starting location for NSD (rNSD) ####
      
      # see troubleshootingNSD.R if you get errors here
      rlocs <- findrloc(lt) 
      
      
      
    #### Redefine some parameters to help avoid convergence issues ####
      
      
      # Allow summer range duration up to 8mos
      refRho <- pEst(u.r = 240)

      # Use that for base model (see troubleshootingNSD.R if errors. Warnings are OK.)       
      mBase <- mvmtClass(lt, rloc = rlocs$rloc, p.est = refRho)
      

      # Allow resident up to 8km daily displacement
      refKappa <- pEst(u.k = log(64))
      
      # Use that to update models, where applicable
      mUpd <- refine(mBase, p.est = refKappa)

        
 
    #### Identify top model for each individual ####
      
      # don't consider mixmig or nomad (see thesis appendix s1 for details)
      mTop <- topmvmt(mUpd, omit = c("mixmig", "nomad"))
      
      # identify preliminary top model per indiv
      topMods <- data.frame(elkYr = elkYrs, PrelimClassn = names(mTop))

      # take quick look at prelim classns 
      summary(topMods$PrelimClassn)


	  

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #     
      
        
    
### ### ### ### ### ### ###
####    |PARAMETERS|   ####
### ### ### ### ### ### ###    

      
      
      
      #### Extract parameters from behavior models ####
      
      
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
          
          
          # pull parameters from top models
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

      
      
      #### Use parameters to reclassify behaviors when applicable ####
      
          
          # starting with model parameters
          reclass <- params %>%
            # if "resident" moved >900km2, reclassify as Migrant (these are actually mixed migrants)
            mutate(Reclass = ifelse(Model == "resident" & gamma > 900, "migrant", Model)) %>%
            # if animal "dispersed" or "migrated" <6.7 km, reclassify as Resident
            mutate(Reclass = ifelse(
              Reclass == "disperser" &  delta < 45 | 
              Reclass == "migrant" &  !is.na(delta) & delta < 45, 
              "resident", Reclass)) %>%
            # if animal "migrated" < 8.7km or "resided" within > 6.7km, reclassify as Other
            mutate(Reclass = ifelse(Reclass == "migrant" & !is.na(delta) & delta < 75, "other", Reclass)) %>%
            mutate(Reclass = ifelse(Reclass == "resident" & !is.na(gamma) & gamma > 45, "other", Reclass)) %>%  
            # rename dispersers as Other
            mutate(Reclass = ifelse(Reclass == "disperser", "other", Reclass)) %>%
            # if animal "dispersed" or "migrated" after summer, reclassify as Other
            mutate(Reclass = ifelse(!is.na(theta) & theta > 270, "other", Reclass)) 




  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #     
  

    
### ### ### ### ### ###
####  |NSD PLOTS|  ####
### ### ### ### ### ###     

        
        
      ## For visual checks to verify that the updated behavior 
      ## classifications make sense based on the locations

      
      #### Define number of plots to create ####
          
        numPlots <- nrow(elkYrs)
        myPlots <- vector(numPlots, mode='list')
      
      
      
      ##### Create a plot for each individual ####
      
        for(i in 1:numPlots) {
          plot(mUpd[[i]])
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
        
        
        
        #### Herd-level summaries - Ppns of behavs ####
        
          # by herd
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
          
          
          # by herd-year
          herdYrBehav <- elkYrHerd %>%
            left_join(behav, by = "elkYr") %>%
            group_by(herdYr) %>%
            summarise(nIndivs = n(),
      	              nMig = length(which(Behav == "migrant")),
      	              nRes = length(which(Behav == "resident")),
      	              nOth = length(which(Behav == "other"))) %>%
      	    mutate(ppnMig = round(nMig/nIndivs, digits = 2),
      	           ppnRes = round(nRes/nIndivs, digits =2),
      	           ppnOth = round(nOth/nIndivs, digits = 2)) %>%
      	    dplyr::select(herdYr, ppnMig, ppnRes, ppnOth, nIndivs)
          write.csv(herdYrBehav, "behavPerHerdYr.csv", row.names=F)
          
        
        
        
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #     
  
          
    
    
### ### ### ### ### ### ### ### ###
####    |SEASONAL SUMMARIES|   ####
### ### ### ### ### ### ### ### ###
	  

        
        
    #### Determine spring and fall movement dates for migrants  ####
        
        
        # identify mixed migrants (can't use their movement dates)
        indivsMix <- reclass %>%
          filter(Reclass == "migrant" & gamma > 900) %>%
          dplyr::select(elkYr)

        
        # identify migrants
        indivsMig <- reclass %>%
          filter(Reclass == "migrant") %>%
          # but not mixed migrants bc their dates make no sense
          anti_join(indivsMix, by = "elkYr") %>%
          dplyr::select(elkYr)  
        indexMig <- which(elkYrs$elkYr %in% indivsMig$elkYr)

        
        # extract movement dates from migrant models
        datesModels <- list()
        for (i in 1:length(indexMig)) {
          datesModels[i] <- mvmt2dt(mUpd[[indexMig[i]]])
          names(datesModels)[[i]] <- elkYrs[indexMig[i], "elkYr"]
        }
        

        # create dataframe to store properly formatted date data
        datesMvmt <- as.data.frame(matrix(ncol = 5, nrow = 0))
        colnames(datesMvmt) <- c("elkYr", "end1", "end2", "str1", "str2")
        
        
        # Format & store movement dates for all migrants
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
          datesMvmt <- bind_rows(datesMvmt, dat)
          
        }
           
        
        # Calculate movement durations, seasonal dates, and distance moved for migrants
        datesMigrants <- datesMvmt %>%
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
          # put distance in km (not km^2); round summer duration to whole number
          mutate(
            dist = sqrt(as.numeric(round(dist, 2))), 
            smrDr = as.numeric(round(smrDr))) %>%
          # add herd and herd-year to facilitate later herd summaries etc
          left_join(elkYrHerd, by = "elkYr") %>%
          mutate(yr = substr(elkYr, nchar(elkYr)-3, nchar(elkYr)))
        
        

    #### Herd-level summaries - movement dates, durations, and distances moved  ####  
        
        
        
        # Per herd-year        
        dateDistHerdYr <- datesMigrants %>%
          group_by(herdYr) %>%
          summarise(
            nMig = n(),
            sprStartMean = mean(sprSt),
            sprStartSd = round(as.numeric(sd(sprSt))),
            sprEndMean = mean(sprEn),
            sprEndSd = round(as.numeric(sd(sprEn))),
            sprDurMean = round(mean(sprDr)),
            sprDurSd = round(sd(sprDr)),
            fallStartMean = mean(fallSt),
            fallStartSd = round(as.numeric(sd(fallSt))),
            fallEndMean = mean(fallEn),
            fallEndSd = round(as.numeric(sd(fallEn))),
            fallDurMean = round(mean(fallDr)),
            fallDurSd = round(sd(fallDr)),
            smrDurMean = round(mean(smrDr)),
            smrDurSd = round(sd(smrDr)),
            distMean = round(mean(dist), 2),
            distSd = round(sd(dist), 2)) %>%
          left_join(dplyr::select(herdYrBehav, herdYr, nIndivs), by = "herdYr") %>%
          # order columns
          dplyr::select(c(herdYr, nMig, nIndivs, 
            sprStartMean, sprEndMean, fallStartMean, fallEndMean, 
            sprDurMean, fallDurMean, smrDurMean, distMean,
            sprStartSd, sprEndSd, sprDurSd, fallStartSd, fallEndSd, fallDurSd,
            smrDurSd, distSd)) 
        # export
        write.csv(dateDistHerdYr, "herdYrDatesDist.csv", row.names = F)
        

        
        # Per herd
        dateDistHerd <- dateDistHerdYr %>%
          # only keep relevant columns
          dplyr::select(
            herdYr, nMig, nIndivs, sprStartMean, sprEndMean, sprDurMean, 
            fallStartMean, fallEndMean, fallDurMean, smrDurMean, distMean) %>%
          # convert dates to dayofyear (for averaging across years)
          mutate(
            Herd = substr(herdYr, 1, nchar(herdYr)-5),
            sprStartMean = as.POSIXlt(sprStartMean)$yday,
            sprEndMean = as.POSIXlt(sprEndMean)$yday,
            fallStartMean = as.POSIXlt(fallStartMean)$yday,
            fallEndMean = as.POSIXlt(fallEndMean)$yday) %>%
          # fix fall return dates in january (add 365 days to the dayofyear)
          # otherwise averaging dates can make fall migration end before it starts
          mutate(fallEndMean = ifelse(fallEndMean < 20, fallEndMean + 365, fallEndMean)) %>%
          group_by(Herd) %>%
          # average herd dates and durations across herd-years (weighted by # of migrants in each yr)
          summarise(
            nMigs = sum(nMig),
            nIndivs = sum(nIndivs),
            nYrs = n(),
            sprStartMean = weighted.mean(sprStartMean, nMig),
            sprEndMean = weighted.mean(sprEndMean, nMig),
            fallStartMean = weighted.mean(fallStartMean, nMig),
            fallEndMean = weighted.mean(fallEndMean, nMig),
            sprDurMean = round(weighted.mean(sprDurMean, nMig)),
            fallDurMean = round(weighted.mean(fallDurMean, nMig)),
            smrDurMean = round(weighted.mean(smrDurMean, nMig)),
            winDurMean = I(365-sprDurMean-fallDurMean-smrDurMean),
            distMean = round(weighted.mean(distMean, nMig))) %>%
          # format averaged dates as month-year rather than dayofyear
          mutate(
            sprStartMean = format(strptime(sprStartMean, format = "%j"), format = "%m-%d"),
            sprEndMean = format(strptime(sprEndMean, format = "%j"), format = "%m-%d"),
            fallStartMean = format(strptime(fallStartMean, format = "%j"), format = "%m-%d"),
            fallEndMean = format(strptime(fallEndMean, format = "%j"), format = "%m-%d"))
        
        # export
        write.csv(dateDistHerd, "herdDatesDist.csv", row.names = F)       
 
         


         
        
    #### Define seasons for all individuals  #### 

               
        # start with behavior classifications
        datesIndivs <- reclass %>%
          # make the behavior label less ambiguous
          rename(behav = Reclass) %>%
          # add herd and herd-year
          left_join(elkYrHerd, by = "elkYr") %>%   
          # indicate that the elk was included in behavior analysis
          mutate(inBehavAnalysis = 1) %>%
          # add elk that weren't included in behavior analysis
          bind_rows(elkYrHerdNoBehav) %>%
          left_join(dateDistHerd, by = "Herd") %>%
          # and indicate that they weren't included
          mutate(inBehavAnalysis = ifelse(is.na(inBehavAnalysis), 0, 1)) %>%
          # add individuals' dates if they migrated
          left_join(datesMigrants, by = c("elkYr", "Herd", "herdYr")) %>%
          # define spring and fall dates for each individual
          mutate(
            # if migrant, use the individual's movement date
            sprStart = ifelse(!is.na(sprSt), as.character(sprSt), 
              # otherwise use the herd average date (with correct year)
              paste(substr(elkYr, nchar(elkYr)-3, nchar(elkYr)), sprStartMean, sep = "-")),
            sprEnd = ifelse(!is.na(sprEn), as.character(sprEn), 
              paste(substr(elkYr, nchar(elkYr)-3, nchar(elkYr)), sprEndMean, sep = "-")),
            fallStart = ifelse(!is.na(fallSt), as.character(fallSt), 
              paste(substr(elkYr, nchar(elkYr)-3, nchar(elkYr)), fallStartMean, sep = "-")),
            fallEnd = ifelse(!is.na(fallEn), as.character(fallEn), 
              paste(substr(elkYr, nchar(elkYr)-3, nchar(elkYr)), fallEndMean, sep = "-")),
            # format those dates as dates
            sprStart = as.Date(sprStart, format = "%Y-%m-%d"),
            sprEnd = as.Date(sprEnd, format = "%Y-%m-%d"),
            fallStart = as.Date(fallStart, format = "%Y-%m-%d"),
            fallEnd = as.Date(fallEnd, format = "%Y-%m-%d"),
            # and use them to define winter and summer
            smrStart = sprEnd+1,
            smrEnd = fallStart-1,
            win1End = sprStart-1,            
            win2Start = fallEnd+1,
            win1Start = win2Start-365) %>%
          # remove extraneous columns & organize
          dplyr::select(elkYr, behav, Herd, herdYr, inBehavAnalysis,
            win1Start, win1End, sprStart, sprEnd, smrStart, smrEnd, fallStart, fallEnd, win2Start, dist)
          
        # export
        write.csv(datesIndivs, "indivDates.csv", row.names = F)
        
      
      
      
      #### Seasonal locations for all recorded collar data ####
      
      
        # identify seasons and classify locations accordingly
        locsSeasons <- locsFormat %>%
          left_join(datesIndivs, by = c("elkYr", "Herd")) %>%
          # label each location with season based on date
          mutate(Season = ifelse(Date >= win1Start & Date <= win1End, "Winter",
            ifelse(Date >= sprStart & Date <= sprEnd, "Spring",
              ifelse(Date >= smrStart & Date <= smrEnd, "Summer", "Fall")))) %>%
          # remove extraneous columns & organize
          dplyr::select(elkYr, herdYr, Herd, AnimalID, CollarID, inBehavAnalysis, behav,
            Date, Time, Latitude, Longitude, Season) 
        
        #export
        write.csv(locsSeasons, "locsSeasonsAll.csv", row.names = F)        

      

      #### Seasonal locations only for elk with at least 20 locs/season ####
        
      
        # identify seasons and classify locations accordingly
        locsSeasonsSubset <- locsFormat %>%
          left_join(datesIndivs, by = c("elkYr", "Herd")) %>%
          # label each location with season based on date
          mutate(Season = ifelse(Date >= win1Start & Date <= win1End, "Winter",
            ifelse(Date >= sprStart & Date <= sprEnd, "Spring",
              ifelse(Date >= smrStart & Date <= smrEnd, "Summer", "Fall")))) %>%
          # only keep indivs with at least 20 locations recorded during a season
          group_by(elkYr, Season) %>%
          filter(n() >= 20) %>%
          ungroup() %>%
          # remove extraneous columns & organize
          dplyr::select(elkYr, herdYr, Herd, AnimalID, CollarID, inBehavAnalysis, behav,
            Date, Time, Latitude, Longitude, Season) %>%
          droplevels() 
      
        #export
        write.csv(locsSeasonsSubset, "locsSeasonsSubset.csv", row.names = F)       
      
 
           
      #### Seasonal locations per herd (export csv's)  ####
      
        for (i in 1:nrow(herds)) {
          # for each herd
          h <- herds$Herd[i]
          # pull locs for just that herd
          dat <- filter(locsSeasonsSubset, Herd == h)
          # identify herd name without any spaces
          outName <- gsub("\\s", "", h)
          # name file locs + herd name
          outPath <- paste0("locs", outName, ".csv")
          # export
          write.csv(dat, file = outPath, row.names = F)
        }
            
      
      

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #     
  

          
    
    
### ### ### ### ###  ### ### ### ###
####    |VOLUME INTERSECTION|   ####
### ### ### ### ###  ### ### ### ###
      
      

    #### Identify locations to estimate winter and summer ranges with ####    
      
 
      # winter locations
      viLocsWin <- locsSeasons %>%
        # subset to only elk that were included in the behavioral analysis
        filter(inBehavAnalysis == 1) %>%
        # add herd-averaged movement dates (to determine minimum winter timeframe)
        left_join(dplyr::select(dateDistHerd, Herd, sprStartMean), by = "Herd") %>%
        # add correct year to herd-averaged date
        mutate(
          yearElk = substr(elkYr, nchar(elkYr)-3, nchar(elkYr)),
          sprStart = paste(yearElk, sprStartMean, sep = "-"),
          winEnd = as.Date(as.Date(sprStart)-1)) %>%
        # calculate length of winter for each individual
        group_by(elkYr) %>%
        mutate(
          day1 = min(Date),
          winLgth = as.numeric(winEnd-day1)) %>%
        ungroup() %>%
        # use same length of time to define winter for each indiv (18 days when i ran it)
        filter(Date >= day1 & Date <= day1+min(winLgth)) %>%
        # label as winter
        mutate(seasonVI = "Winter") %>%
        # remove extra columns to allow join with summer locs
        dplyr::select(-c(Season, sprStartMean, yearElk, sprStart, winEnd, day1, winLgth)) %>%
        distinct()
        
        
      # summer locations
      viLocsSmr <- locsSeasons %>%
        # subset to only elk that were included in the behavioral analysis
        filter(inBehavAnalysis == 1) %>%
        # add herd-averaged movement dates (to determine minimum summer timeframe)
        left_join(dplyr::select(dateDistHerd, Herd, smrDurMean), by = "Herd") %>%  
        # add individual summer start dates
        left_join(dplyr::select(datesIndivs, elkYr, smrStart)) %>%
        # keep same length of time of summer locs for each indiv (32 days when i ran it)
        filter(Date >= smrStart & Date <= smrStart + min(dateDistHerd$smrDurMean)) %>%
        # label as summer
        mutate(seasonVI = "Summer") %>%
        # remove extra columns to allow join with winter locs
        dplyr::select(-c(Season, smrDurMean, smrStart)) %>%
        distinct()
  
      # combine winter and summer locations
      viLocs <- bind_rows(viLocsWin, viLocsSmr)

      
      
      
    #### Calculate volume intersection between seasonal ranges ####    
      
      
      # prep data to loop through indivs
      viIndivs <- unique(viLocs$elkYr)
      viDat <- data.frame(matrix(ncol = 3, nrow = length(viIndivs))) #blank df
      colnames(viDat) <- c("elkYr", "VI95", "VI50")
      
      # calculate 95% and 50% VI for each individual
      for(i in 1:length(viIndivs)) {
        elk <- viIndivs[i]
        
        #subset individual locations
        tempDat <- subset(viLocs, elkYr == elk) 
        
        #Get xy points, write to dataframe, to spatial data frame, to stateplane projection
        xy <- data.frame("x"=tempDat$Longitude,"y"=tempDat$Latitude)
        xy.spdf.ll <- SpatialPointsDataFrame(xy, tempDat, proj4string = latlong)
        xy.spdf.sp <- spTransform(xy.spdf.ll,stateplane)
      
        #calculate kdes and volume intersections
        kud <- kernelUD(xy.spdf.sp[,"seasonVI"], h = "href", same4all=TRUE) #LSCV not alwys converged
        vol95 <- kerneloverlaphr(kud, method = "VI", percent = 95, conditional = TRUE)
        vol50 <- kerneloverlaphr(kud, method = "VI", percent = 50, conditional = TRUE)
        
        #store results
        viDat[[i,"elkYr"]] <- elk
        viDat[[i,"VI95"]] <- vol95[2,1]
        viDat[[i,"VI50"]] <- vol50[2,1]
      }    

  
      # format indiv data (dates, durations, distances, VI)
      indivDat <- viDat %>%
        # round VI
        mutate(VI95 = round(VI95, 2), VI50 = round(VI50, 2)) %>%
        # calculate durations
        mutate(
          sprDur = as.numeric(round(sprEnd - sprStart)),
          fallDur = as.numeric(round(fallEnd - fallStart)),
          smrDur = as.numeric(round(smrEnd - smrStart))) %>%
        dplyr::select(c(elkYr, Herd, herdYr, behav, 
          sprStart, sprEnd, fallStart, fallEnd,
          sprDur, fallDur, smrDur,
          dist, VI95, VI50))
      write.csv(indivDat, "indivDatesDistVI.csv", row.names = F)
            
        
      
      

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
      
      
          
    
    
### ### ### ### ### ### ### ### ##
####    |SHAPEFILE OUTPUTS|   ####
### ### ### ### ### ### ### ### ##
        
        
      # increase memory limit to handle large shapefiles
      memory.limit(size = 7500000)
      

      # export csvs and shps of all seasonal locs per herd, 
      # only using elk with >=20 locs in a season
      # (to use all elk regardless of #locs/season, 
      #  replace "locsSeasonsSubset" with "locsSeasons")
      
      
      

      
      # make seasonal locs spatial
      locsSeasonsSpat <- SpatialPointsDataFrame(
        data.frame("x"=locsSeasonsSubset$Longitude,"y"=locsSeasonsSubset$Latitude), 
        locsSeasonsSubset, proj4string = latlong)
      
      
      #### Shapefile of all seasonal locations #### 
      dir.create("Shapefiles")
      writeOGR(locsSeasonsSpat,
               dsn = "./Shapefiles",
               layer = "SeasonalLocs",
               driver = "ESRI Shapefile",
               overwrite_layer = TRUE) 
      
      
      #### Shapefiles of seasonal locations per herd #### 
      for (i in 1:nrow(herds)) {
        # for each herd
        h <- herds$Herd[i]
        dat <- filter(locsSeasons, Herd == h)
        # make its locations spatial
        datSpat <- SpatialPointsDataFrame(
          data.frame("x"=dat$Longitude,"y"=dat$Latitude), 
          dat, proj4string = latlong)
        # identify herd name without any spaces
        outName <- gsub("\\s", "", h)
        # export shp with filename locs + herdname
        writeOGR(datSpat,
                 dsn = "./Shapefiles",
                 layer = paste0("locs", outName),
                 driver = "ESRI Shapefile",
                 overwrite_layer = TRUE) 
      }      

      
     

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #     
  

        # save.image(file = "MTmig-prelim.RData")  