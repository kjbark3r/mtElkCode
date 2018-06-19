## playtime



#### quit using setwd() like a f*ing caveman ####

  # following guidance from https://github.com/jennybc/here_here#readme
  install.packages("here")
  library(here)
  here("testdat", "alllocs-equalsampling.csv")
  # no, first argument is supposed to be the folder name i think
  ?here
  here("Migration", "alllocs-equalsampling.csv")
  # nice, yep, only that's not where that file lives
  here("DatabasesEtc", "collardata-locsonly-equalsampling.csv")
  # oh wait that's huge, let's try something smaller to load
  test <- here("Migration", "behav-summaries.csv")
  # oh that just sets the filepath; it doesn't load the data.
  # dur, you didn't tell it to read it in
  testdat <- read.csv(here("Migration", "behav-per-popn.csv"))
  # ahh ok
  # so to implement for migcode, use something like
  locs <- read.csv(here("NameOfFolderWhereYourDataIsStored", "filename.csv"))
  
  
  
  
#### sessionInfo ####
  
  ?sessionInfo()
  sessionInfo() 
  # gives R version, packages and versions
  # useful if version changes in future break things
  # you know which ones to go back to
  # ok, so can you store it?
  writeLines(capture.output(sessionInfo()), "sessionInfo.txt")
  # cool.
  # capture.output() makes the output in the console into a character vector
  # and writeLines() stores it
  
  
  
  
#### can here() source a script?? ####
  
  # how you did it before: source("../zMisc/pairs-panels.R")
  here("zMisc", "pairs-panels.R")
  source(here("Migration", "pairs-panels.R"))
  ?pairs.panels
  pairs.panels(testdat)
  # niiiiice
  
  # oooh i could put code to install packages etc on my github
  # and kelly or whoever could source it right from there,
  # that way it's just a one-liner in the actual code, eg
  devtools::install_github("kristinjbarker/installElkMigPkgs")
  
  
  
  
#### figure out which of your 2 code options installs and loads packages fastest ####
  
  install.packages("tictoc")
  library(tictoc)
  
  tic("is this the label?")
  print("coding is fun")
  toc()
  
  tic("pairspanels")
  pairs.panels(testdat)
  toc()
  
  
  
  ## option 1 nowak (tidyverse)
 
 
  tic("nowak - total")
  
    tic("nowak - install")
  
      please_install <- function(pkgs, install_fun = install.packages) {
          if (length(pkgs) == 0) {
            return(invisible())
          }
          if (!interactive()) {
            stop("Please run in interactive session", call. = FALSE)
          }
    
          title <- paste0(
            "Ok to install these packges?\n",
            paste("* ", pkgs, collapse = "\n")
          )
          ok <- menu(c("Yes", "No"), title = title) == 1
    
          if (!ok) {
            return(invisible())
          }
    
          install_fun(pkgs)
        }
        
        #  Do you have these?
        pkgsNeeded <- c("dplyr", "tidyr", "adehabitatHR")
        
        #  Horribly slow
        have <- rownames(installed.packages())
        needed <- setdiff(pkgsNeeded, have)
    
      please_install(needed)
      
    toc()
    
    tic("my lame install")
    
      library(dplyr)
      library(tidyr)
      library(adehabitatHR)
    
    toc()
  
  toc()
  
  ## remove packages from workspace
  detach("package:dplyr", unload=TRUE)
  detach("package:tidyr", unload=TRUE)
  detach("package:adehabitatHR", unload=TRUE)
  
  
  # option 2 dan (Steven Worthington)
  tic("dan")
  
      ipak <- function(pkg){
        new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
        if (length(new.pkg)) 
            install.packages(new.pkg, dependencies = TRUE)
        sapply(pkg, require, character.only = TRUE)
    }
    
    # usage
    packages <- c("dplyr", "tidyr", "adehabitatHR")
    ipak(packages)
  
  toc()
  
  # steven worthington ftw
  
  
  
  
#### testing sourcing that code straight from git
  
  source("github.com/kjbark3r/zMisc/blob/master/packages-ElkMigMT.R")
  ?source
  # y'know what... i'm gonna make a fucking package today. 
  
  
  
  
#### baby's first r package ####
  
  # following guidance from https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/
  
  library("devtools")
  library("roxygen2")
  
  setwd("C:\\Users\\kristin\\Documents\\MontanaElkCode\\TestRuns")
  create("pkgTest")
  
  
  setwd("./pkgTest")
  document()
  
  
  setwd("C:\\Users\\kristin\\Documents\\MontanaElkCode\\TestRuns")
  install("pkgTest")
  ?ipak
  
  
  ## after you do it run this
      
          
    # packages lists all the packages needed for the MT elk migration code
    # and ipak function installs and loads them
    packages <- c("dplyr", "tidyr", "adehabitatHR")
    ipak(packages)
    
    # oh. my. fuck.
    
    
  ## now stick it on github and try running it from there.
    
    # ok, made new repo, copied entire "pkgTest" folder contents into it, and committed.
    # closing and reopening rstudio to see if running it works as expected
    
    # run this line if you don't already have the devtools package (or if you're not sure)
    install.packages("devtools")
    
    devtools::install_github("kjbark3r/pkgTest")
    require(pkgTest)
    packages <- c("dplyr", "tidyr", "adehabitatHR")
    ipak(packages)  
    # holy fucking shitballs. yessssssssssss. 
    

####  use here() to read in access database? ####
    

    channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                dbq=C:/Users/kristin/Documents/DatabasesEtc/Statewide/Elk/Statewide_Elk_GPS.accdb") 
    
    here() # ok this defaults to whereever you opened the code from? or...?
    dr_here(show_reason=TRUE)

    # ah damn, this would get weird if someone already had r open with a wd() set. 
    # abort. 
    # switch to using rproj instead.
    
    
    
#### so pulling database out of the pre-defined project folder ####
    
    # opened this code from within rproject folder
    getwd()
    # cool, wd set to project folder
    
    channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
      dbq = paste(getwd(), dataFile, sep = "/")") #n
    
    paste(getwd(), dataFile, sep = "/") # maybe quotes are the issue
    paste(noquote(getwd(), dataFile, sep = "/")) # n
    noquote(paste(getwd(), dataFile, sep = "/")) # y
    
    channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
      dbq = noquote(paste(getwd(), dataFile, sep = "/"))") #n

    channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                dbq=C:/Users/kristin/Documents/MontanaElkCode/mtElkCode/Statewide_Elk_GPS_copy.accdb") #y
    
    channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
      dbq = as.character(noquote(paste(getwd(), dataFile, sep = "/")))") #n
    
    ?odbcDriverConnect
    
    channel2 <- odbcConnect("Statewide_Elk_GPS_copy.accdb") #n
    channel2 <- odbcConnect("Statewide_Elk_GPS_copy") #n    
    channel2 <- odbcConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)}; dbq = Statewide_Elk_GPS_copy.accdb") #n    
    channel2 <- odbcConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)}; dbq = Statewide_Elk_GPS_copy") #n 
    channel2 <- odbcConnectAccess("Statewide_Elk_GPS_copy.accdb") # finally just the 32-bit error msg  
    # but doesn't work in 32-bit, damn
    channel2 <- odbcConnectAccess("Statewide_Elk_GPS_copy") # one fewer error...
   
    dataFile <- "Statewide_Elk_GPS_copy.accdb"
    filePath <- noquote(paste(getwd(), dataFile, sep = "/"))
    filePath2 <- paste(getwd(), dataFile, sep = "/")
    
    channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                dbq=filePath")
    channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                dbq=filepath2")   
    channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                dbq=paste(filepath")    
    channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                dbq=paste(filepath2")    
    channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                dbq=print(filepath")    
    channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                dbq=paste(filepath2") 
    # son of a...
    
    channel <- odbcConnectAccess("C:/Users/kristin/Documents/MontanaElkCode/mtElkCode/Statewide_Elk_GPS_copy.accdb") #n
    channel <- odbcConnectAccess("C:/Users/kristin/Documents/MontanaElkCode/mtElkCode/Statewide_Elk_GPS_copy") #n
    

    # exciting new problem, this works in 64-bit but not 32-bit. wtf.
    channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                dbq=C:/Users/kristin/Documents/MontanaElkCode/mtElkCode/Statewide_Elk_GPS_copy.accdb") #y
    rawlocs <- sqlQuery(channel, paste("select * from ElkGPS"))
    # mmk yep guess it's not worth sinking time into, can't get the pasting to work for the life of me
    # come back to it if you have spare time
      
      
      
#### use pdf() with here() to store nsd plots? ####
    
    
    getwd() # nice, still haven't set it
    
    # use it your normal way
      
      # pdf() to open the file
      pdf('./MontanaElkCode/TestRuns/t1.pdf')
      # print this to the file
      boxplot(Age ~ Herd, data = test)
      # close the file
      graphics.off()
    
      
    # now with here
      
      outputFolder <- "MontanaElkCode"
      pdf(here(outputFolder, 't2.pdf'))
      boxplot(Age ~ Herd, data = test)
      graphics.off()   
      # damn i'm good
      
      
    # now make it work with a subfolder
      
      outputFolder <- c("MontanaElkCode", "TestRuns")
      pdf(here(outputFolder, 't2.pdf'))
      boxplot(Age ~ Herd, data = test)
      graphics.off()
      # newp, concatenating the 2 names doesn't work
      
      # verify it does work if you type it in normally
      pdf(here("MontanaElkCode", "TestRuns", "t2.pdf"))
      boxplot(Age ~ Herd, data = test)
      graphics.off()   
      # ok, yep
      
      tf <- c("MontanaElkCode", "TestRuns")
      outputFolder <- cat(paste(shQuote(tf, type="cmd"), collapse=", ")) # no
      outputFolder <- as.character(cat(paste(shQuote(tf, type="cmd"), collapse=", "))) # no
      paste(as.character(tf), collapse = ", ", sep = "") # no
      outputFolder <- as.character(paste(shQuote(tf, type="cmd"), collapse=", ")) #  no
      paste("\"",as.character(tf),"\"",collapse=", ",sep="") # no
      cat(paste("\"",as.character(tf),"\"",collapse=", ",sep="")) # yes but can you store it?
      outputFolder <- cat(paste("\"",as.character(tf),"\"",collapse=", ",sep="")) # no, only prints
      outputFolder # mmk... do you NEED to store it?
      
      pdf(here(cat(paste("\"",as.character(tf),"\"",collapse=", ",sep="")) , "t2.pdf")) # no
      pdf(here(paste("\"",as.character(tf),"\"",collapse=", ",sep="")) , "t2.pdf") # no
      
      ## ok you're burning too much time; come back to this
      

#### fixing NA date-times from elk db, of course ####      
      
      length(which(is.na(locsRaw$Date))) # 0
      length(which(is.na(locsRaw$Time))) # 3737
      length(which(is.na(locsRaw$DT))) # 73806
      length(which(is.na(locsRaw$Time) & is.na(locsRaw$DT))) # 3737, good
      
      z <- locsRaw[is.na(locsRaw$DT), ]
      View(z)
      zz <- locsRaw[is.na(locsRaw$Time), ]
      View(zz)
      unique(zz$AnimalID)
      # ok NA times are just a one-off from one indiv's collar
      
      # NA DTs should be fixable bc date and time still exist for them
      
      strftime(z[1:3, "Time"])
      strftime(z[1:3, "Time"], format = "%H:%M:%S")
      z$timeTest <- strftime(z$Time, format = "%H:%M:%S")      
      any(is.na(z$timeTest))
      length(which(is.na(z$timeTest))) # ok those are just the one screwy one
      # so the exact same code worked just fine for these indivs in their own df
      # must have something to do with my date formatting beforehand.
      # m yep that fixed it
      
            
      
#### starting with summary stuff ####
      
      # use mvmt2dt() to get durations
      
      mvmt2dt(mtop) # nope, need the mvmt/mvmts class
      # hm, so have to use the object that doesn't incl the top identified model?
      test <- mvmt2dt(mref1)
      # error... so can't use the refined model, that seems sketchy
      # oh actually for us this is luckily ok
      # because the refined model only affected a resident parameter
      # which isn't important for this analysis
      test <- mvmt2dt(mbase)
      # only warning, no error
      # these warnings are for animals that have overlapping mvmts
      # or couldn't fit a migrant model (which is fine bc we'll define their dates separately)
      # does retain all indivs even the warning ones
      # so this is a list of dataframes, one per indiv, with day from start and date/times assocd with mvmts
      

    ## turn confusing mvmt2dt list into pretty dataframe
      
      length(test) # 314, this is number indivs (&associated dfs)
      names(test) # schweet
      t2 <- data.frame(AnimalID = names(test))
      View(t2) # perfect, now pull the info out of each df
      
      t2 <- data.frame(str1 = lapply(test, "str1", FUN = mean)) # this makes them columns
      
      # pull from one list element
      test[[1]] #y
      test[[1]]$date #y
      test[[1]]$date[1] #y
      test[[1]]$date["str2"] #N
      test[[1]]$date[rownames(test)=="str1"] #N
      test[1]
      test[[1]][1,1] #y
      test[[1]][rownames(test) == "str1",]
      test[[1]]["str1",1] #y!
      test[[1]]["str1", "date"] #f y!
      
      # try df, subset to just a list of 3
      t <- test[1:3]
      
      # blank df
      t2 <- data.frame(
        AnimalID = as.character(),
        sprStart = as.character(),
        sprEnd = as.character(),
        fallStart = as.character(),
        fallEnd = as.character(),
        stringsAsFactors = FALSE
      )


      t2 <- data.frame(matrix(nrow = length(t)), AnimalID = NA)
      
      t2 <- data.frame(matrix(NULL)) #n
      t2 <- data.frame(NULL) #n
      
      t2 <- data.frame(matrix(nrow = length(t), ncol = 5))
      colnames(t2) = c("AnimalID", "sprStart", "sprEnd", "fallStart", "fallEnd")
      
      # gah!
      
      
      # closest so far:
      
      t2 <- data.frame(matrix(nrow = length(t)))
      
      for (i in 1:length(t)) {
        t2$AnimalID[i] = names(test)[i]
        t2$sprStart[i] = test[[i]]["str1", "date"]
        t2$sprEnd[i] = test[[i]]["end1", "date"]
        t2$fallStart[i] = test[[i]]["str2", "date"]
        t2$fallEnd[i] = test[[i]]["end2", "date"]
      }

      ## ok time to stop
      ## problems to solve tomorrow:
        ## 1. initializing the dataframe correctly
          ## (correct number of rows with column names, or
          ## might have to ave one extra column at start that you
          ## delete at the end a la above
        ## 2. pulling in the date/times correctly
          ## right now they're weird strings of numbers, not posix
            ## considerL
              ## just pulling in the initial date strings
              ## formatting in advance
              ## something more clever
      
      
      t2 <- data.frame()[1:length(t),]
      for (i in 1:length(t)) {
        t2$AnimalID[i] = names(test)[i]
        t2$sprStart[i] = as.character(test[[i]]["str1", "date"])
        t2$sprEnd[i] = as.character(test[[i]]["end1", "date"])
        t2$fallStart[i] = as.character(test[[i]]["str2", "date"])
        t2$fallEnd[i] = as.character(test[[i]]["end2", "date"])        
      }
      
      ## hahahahaha wow good thing i stopped
      ## that took literally <5min on a fresh brain
      
      ## congratulations kristin, you have earned 10 mins of playtime
      ## to try to figure out how to do that using apply instead of a loop
      
      
      t2 <- data.frame()[1:length(t),]
      t2$AnimalID <- names(t)
      t2$sprStart <- lapply(t,function(x) x["str1", "date"]) 
      # holy fuckballs, did that actually work?!
      t2$sprStart <- lapply(t, as.character(x) x["str1", "date"]) # n
      t2$sprStart <- lapply(t, as.character(x), x["str1", "date"]) # n    
      t2$sprStart <- lapply(t,function(x) as.character(x["str1", "date"])) #y!
      # aaaaamazing
      # and with >3 minutes to spare
      
      
      test <- mvmt2dt(mbase)
      test2 <- data.frame()[1:length(test),]
      test2$AnimalID <- names(test)
      test2$sprStart <- lapply(test, function(x) as.character(substr(x["str1", "date"], 0, 10)))
      test2$sprEnd <- lapply(test, function(x) as.character(substr(x["end1", "date"], 0, 10)))
      test2$fallStart <- lapply(test, function(x) as.character(substr(x["str2", "date"], 0, 10)))
      test2$fallEnd <- lapply(test, function(x) as.character(substr(x["end2", "date"], 0, 10)))
      test2 <- data.frame(test2)
      
      # ah shit, using lapply turns the dataframe back into a list, gross
      any(is.na(test))
      View(test2)
      # bc the row names are NA (?!)
      # ok sadly we are out of time for this game, going back to the sad for loop
      
      
  
      test2 <- data.frame()[1:length(test),]
      for (i in 1:length(test)) {
        test2$AnimalID[i] = names(test)[i]
        #if(is.null(test[[i]])) next
        test2$sprStart[i] = as.character(substr(test[[i]]["str1", "date"], 0, 10))
        test2$sprEnd[i] = as.character(substr(test[[i]]["end1", "date"], 0, 10))
        test2$fallStart[i] = as.character(substr(test[[i]]["str2", "date"], 0, 10))
        test2$fallEnd[i] = as.character(substr(test[[i]]["end2", "date"], 0, 10)) 
        
      }
      
      # ohhhh no the reason lapply failed is because some dataframes within that list are NULL
      # which i assume is because their movement dates couldn't be calculated bc that model never converged
      # which is ok, i was going to fill them in manually, but need to tell r to ignore them
      # seems painful/weird with lapply, so sticking with lame for loop 
      
      
      test <- mvmt2dt(mtop)      
      test2 <- data.frame()[1:length(test),]
      for (i in 1:length(test)) {
        test2$AnimalID[i] = names(test)[i]
        if(!is.null(test[[i]])) {
        test2$sprStart[i] = as.character(substr(test[[i]]["str1", "date"], 0, 10))
        test2$sprEnd[i] = as.character(substr(test[[i]]["end1", "date"], 0, 10))
        test2$fallStart[i] = as.character(substr(test[[i]]["str2", "date"], 0, 10))
        test2$fallEnd[i] = as.character(substr(test[[i]]["end2", "date"], 0, 10))
        }
      }     
      # fills in the NULL ones with the values from i=1 
      # i don't understand why, need to figure this out...
      
      
      test2 <- data.frame()[1:length(test),]
      for (i in 1:length(test)) {
        test2$AnimalID[i] = names(test)[i]
        if(is.null(test[[i]])) skip {
        test2$sprStart[i] = as.character(substr(test[[i]]["str1", "date"], 0, 10)) 
        test2$sprEnd[i] = as.character(substr(test[[i]]["end1", "date"], 0, 10)) 
        test2$fallStart[i] = as.character(substr(test[[i]]["str2", "date"], 0, 10)) 
        test2$fallEnd[i] = as.character(substr(test[[i]]["end2", "date"], 0, 10))
        }
      } 
      # n, total fail
      
      
      test2 <- data.frame()[1:length(test),]
      for (i in 1:length(test)) {
        test2$AnimalID[i] = names(test)[i]
        if(is.null(test[[i]])) next 
        test2$sprStart[i] = as.character(substr(test[[i]]["str1", "date"], 0, 10)) 
        test2$sprEnd[i] = as.character(substr(test[[i]]["end1", "date"], 0, 10)) 
        test2$fallStart[i] = as.character(substr(test[[i]]["str2", "date"], 0, 10)) 
        test2$fallEnd[i] = as.character(substr(test[[i]]["end2", "date"], 0, 10))
      }        
      # n, fills in from i=1
      
      
      test2 <- data.frame()[1:length(test),]
      for (i in 1:length(test)) {
        if(is.null(test[[i]])) next 
        test2$AnimalID[i] = names(test)[i]
        test2$sprStart[i] = as.character(substr(test[[i]]["str1", "date"], 0, 10)) 
        test2$sprEnd[i] = as.character(substr(test[[i]]["end1", "date"], 0, 10)) 
        test2$fallStart[i] = as.character(substr(test[[i]]["str2", "date"], 0, 10)) 
        test2$fallEnd[i] = as.character(substr(test[[i]]["end2", "date"], 0, 10))
      }     
      # putting if(is.null) before the animalID assignment
      # now fills those rows in with i = 1
      
      ## challenge to solve:
      ## how to fill in rows for the NULL i's
      ## rather than filling with the i = 1 values?
          ## so, figure out why it's pulling i = 1, must skip through to all NULLs first then run the rest
          ## try telling it to fill in rows 2:5 with NA if it's NULL
      
      test2 <- data.frame()[1:length(test),]
      for (i in 1:length(test)) {
        if(is.null(test[[i]])) {
        test2[i,2:5] = NA
        } else {
        test2$AnimalID[i] = names(test)[i]
        test2$sprStart[i] = as.character(substr(test[[i]]["str1", "date"], 0, 10)) 
        test2$sprEnd[i] = as.character(substr(test[[i]]["end1", "date"], 0, 10)) 
        test2$fallStart[i] = as.character(substr(test[[i]]["str2", "date"], 0, 10)) 
        test2$fallEnd[i] = as.character(substr(test[[i]]["end2", "date"], 0, 10))
        }
      }
      #ok that's close, need to keep animalID though

      test2 <- data.frame()[1:length(test),]
      test2$AnimalID = names(test)
      for (i in 1:length(test)) {
        if(is.null(test[[i]])) {
        test2[i,2:5] = NA
        } else {
        test2$sprStart[i] = as.character(substr(test[[i]]["str1", "date"], 0, 10)) 
        test2$sprEnd[i] = as.character(substr(test[[i]]["end1", "date"], 0, 10)) 
        test2$fallStart[i] = as.character(substr(test[[i]]["str2", "date"], 0, 10)) 
        test2$fallEnd[i] = as.character(substr(test[[i]]["end2", "date"], 0, 10))
        }
      }
      #fiiiiinally, sheesh
      
      
    #### output code to copy and replace names of things in ####
      
      
      # get list of movement dates derived from migrant models
      test <- mvmt2dt(mbase) 
      
      # create blank dataframe to store dates in
      test2 <- data.frame()[1:length(test),]
      
      # add all animal IDs to dataframe
      test2$AnimalID = names(test)
      
      # for each indiv's dataframe in the list of mvmt dates,
      for (i in 1:length(test)) {
        
        # if migrant model never converged, make dates NA
        if(is.null(test[[i]])) {
        test2[i,2:5] = NA
        
        # otherwise store start and end dates of spring and fall migration per indiv
        } else {
        test2$sprStart[i] = as.character(substr(test[[i]]["str1", "date"], 0, 10)) 
        test2$sprEnd[i] = as.character(substr(test[[i]]["end1", "date"], 0, 10)) 
        test2$fallStart[i] = as.character(substr(test[[i]]["str2", "date"], 0, 10)) 
        test2$fallEnd[i] = as.character(substr(test[[i]]["end2", "date"], 0, 10))
        }
      }
      
      
      # # # NEXT STEPS HERE ARE # # #
      
      # add behavior classification and herd for each indiv
      # -- need to deal with dates depending what kelly says --
      # format dates as such
      # figure out how to deal with the overlapping dates (30 of these, see warnings)
      # calculate duration of spring and fall migrations
      
      
      