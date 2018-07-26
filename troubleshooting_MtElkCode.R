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
      # ok NA times are just a one-off from one indiv's collar - BROOT1131
      
      # NA DTs should be fixable bc date and time still exist for them
      
      strftime(z[1:3, "Time"])
      strftime(z[1:3, "Time"], format = "%H:%M:%S")
      z$timeTest <- strftime(z$Time, format = "%H:%M:%S")      
      any(is.na(z$timeTest))
      length(which(is.na(z$timeTest))) # ok those are just the one screwy one
      # so the exact same code worked just fine for these indivs in their own df
      # must have something to do with my date formatting beforehand.
      # m yep that fixed it
      
      
      # dealing with that indiv, then...
      z <- locsRaw[is.na(locsRaw$Time), ]
      View(z)      
      unique(z$AnimalID)
      zz <- locsRaw[locsRaw$AnimalID == "BROOT1131",]
      length(which(is.na(zz$Time))); nrow(zz)
      # ok so no times were recorded by that collar
      # emailed kelly, making note, moving on for now.
      
      
      
      # and then it turned out there were more problems
      # fixed BROOT1131 but now realized there are some Times
      # that have a date recorded but no actual time, 
      # those are the remaining NAs.
      
      locsRaw[1070876, 6]
      locsRaw[1070876, "Time"]
      nchar(locsRaw[1070876, "Time"]) # 11
      nchar(locsRaw[1, "Time"]) # 11, newp
      locsRaw[1, "Time"]
      length(locsRaw[1, "Time"]) # 1, newp
      str(locsRaw$Time)
      as.POSIXlt(locsRaw[1, "Time"])
      as.POSIXlt(locsRaw[1, "Time"])$hour # 15, yep
      as.POSIXlt(locsRaw[1070876, "Time"])$hour # hanging. . . 0. newp
      nchar(as.character(locsRaw[1, "Time"])) # 19
      nchar(as.character(locsRaw[1070876, "Time"])) # 10, there we go, sheesh
      
      # pull just the troublemakers
      z <- locsRaw %>%
        mutate(srsly = nchar(as.character(Time)))
      unique(z$srsly) 
      z <- locsRaw %>%
        mutate(srsly = as.character(Time))
      # ok these are just from times recorded as exactly 00:00:00
      # handle that in the code
      
      
      # ook so why are there still dupes? 
      # shouldn't be because of those 00:00:00 times then...
      
      z <- locsModel %>% 
        group_by(AnimalID, DT) %>%
        filter(n() >1)
      # somehow effed up creating dt
      
      # ok i am really not understanding why this isn't working...
      # cut down nrows of data and then mess with it
      z1 <- locsFormat %>%
        # remove NA locations
        filter(!is.na(Latitude)) %>%        
        # only use indivs with at least 9 mos of locs
        group_by(elkYr) %>%
        filter(length(unique(monthLoc)) > 8) %>%
        ungroup() 
      
      z2 <- z1 %>%
        # combine date and time in POSIXct format
        mutate (DT = as.POSIXct(paste(Date, Time, sep = " ")), format = "%Y-%m-%d %H:%M:%S")    
      
      # oh wait is it just the effing levels? shouldn't be but check real quick
      z <- locsFormat %>%
        # remove NA locations
        filter(!is.na(Latitude)) %>%        
        # only use indivs with at least 9 mos of locs
        group_by(elkYr) %>%
        filter(length(unique(monthLoc)) > 8) %>%
        ungroup() %>%
        droplevels() %>%
        mutate (DT = as.POSIXct(paste(Date, Time, sep = " "), format = "%Y-%m-%d %H:%M:%S"))
      # oh good god. fucking stored factor levels. you should really switch to tibbles...

      z1 <- z %>% 
        group_by(AnimalID, DT) %>%
        filter(n() >1)
      droplevels(z1)
      unique(as.POSIXlt(z1$Date)$mon)
      
      ## MOTHERFUCKING PIECE OF SHIT DAYLIGHT SAVING TIME NAs I SHOULD'VE FUCKING KNOWN
      # i am not deleting those locations god damn it
      # ok options to address
        # 1. remove daylight savings NAs at the start (i don't like it)
        # 2. if it's a daylight savigs time NA, change the hour and recreate that DT
          # yeah it'll be a bit of a pain but more responsible to do this way
      
      
      z2 <- filter(z1, is.na(DT))
      unique(as.POSIXlt(z2$Date)$mon)
      # alright they're just the feb/march locs, 
      # this is when clocks spring forward
      # so subtract one hour from times of NA DTs and re-paste
      
      # posixct add 3 hours: + 3*60*60 
      # but don't want it in posixct bc doesn't have date associated, ugh
      z <- locsFormat[1,]
      z$Time
      z$Time-1 # error non-numeric, as expected...
      as.numeric(z$TIme) # numeric(0), that's not gonna work
      # maybe make it ct for now then re-strip time, great
      z$ct <- as.POSIXct(z$Time, format = "%H:%M:%S")
      z$ct
      z$hm <- z$ct - 1*60*60 
      z$hm # ok subtracted 1 hr
      z$Time <- strftime(z$hm, format = "%H:%M:%S")
      z$Time
      z$DT <- as.POSIXct(paste(z$Date, z$Time, sep = " "), format = "%Y-%m-%d %H:%M:%S")
      z$DT
      # ooook worked for normal datetime; now check with daylight savings times
      

      z2$ct <- as.POSIXct(z2$Time, format = "%H:%M:%S")
      #z2$ct
      z2$hm <- z2$ct - 1*60*60 
      #z2$hm # ok subtracted 1 hr
      z2$Time <- strftime(z2$hm, format = "%H:%M:%S")
      #z2$Time
      z2$DT <- as.POSIXct(paste(z2$Date, z2$Time, sep = " "), format = "%Y-%m-%d %H:%M:%S")
      any(is.na(z2$DT)) # there. fuck you DST.
      
      

      
      
#### the one that had missing times ####
      
              
        
        ### KRISTIN REMOVE THIS LINE ~~~~~~~~~~~~~~~~~~~~ ###
        filter(AnimalID != "BROOT1131") 
        ###  AFTER YOU SOLVE THE CASE OF THE MISSING TIMES ~~~~~~~~~~ ##        
        
        
        
        
        
      
#### rLoc error indivs ####
      
      
      e1 <- filter(locsModel, elkYr == "BROOT0015-2011")
      e2 <- filter(locsModel, elkYr == "BROOT1130-2013")
      z1 <- filter(locsModel, elkYr == "140040-2015")
      
      ## take 2, after the great datetime fiasco of 2018
      
      # got 13 errors, first run, trying to resubset random locs and rerun
      # bc only had 2 errors before and don't understand what the issue is
      # if continued issues, options to address are:
        # 1. just force error indivs to use 1st loc
        # 2. do that thing you did before where you iteratively try other locs (huge pain)
        # 3. other??
      
      z <- locsModel[locsModel$elkYr == "SC11050-2016",]
      # hm, looks like DT has one wrong year, which i assume is due to this whole DST BS
      
      
      z <- locsRaw[locsRaw$AnimalID == "SC11050",]
      # run thru first round locsFormat
      z <- z %>%
        mutate(
          # remove messed-up date from Time 
          Time = strftime(Time, format = "%H:%M:%S"),
          # format Date as date
          Date = as.Date(Date),          
          # identify year and month of each location
          yearLoc = substr(Date, 0, 4),
          monthLoc = substr(Date, 6, 7),
          # combine date and time in POSIXct format
          DT = as.POSIXct(paste(Date, Time, sep = " "), format = "%Y-%m-%d %H:%M:%S"),          
          # identify elk-year
          elkYr = paste(AnimalID, yearLoc, sep = "-")
        ) %>%
        filter(elkYr == "SC11050-2016")
      any(is.na(z$DT)) # ok, yes, this is a DST thing
      # need to figure out why it pasted the year wrong
      
      zz <- z[is.na(z$DT),]
      as.POSIXct(paste(zz$Date, 
      strftime((as.POSIXct(zz$Time, format = "%H:%M:%S") - 1*60*60), format = "%H:%M:%S"), 
          sep = " "), format = "%Y-%m-%d %H:%M:%S")
      # that line pasted the year correctly
      
      z1 <- locsFormat[locsFormat$elkYr == "SC11050-2016",]
      
      # ohhh duh i get it
      
      ## this didn't work because it didn't cycle through each NA value
      ## it just replaced them all with the date/time from the first instance
      locsFormat$DT[is.na(locsFormat$DT)] <- as.POSIXct(paste(locsFormat$Date, 
        # by removing one hour from those times
        strftime((as.POSIXct(locsFormat$Time, format = "%H:%M:%S") - 1*60*60), format = "%H:%M:%S"), 
            sep = " "), format = "%Y-%m-%d %H:%M:%S")  
      
      
      # ok wtf, why does this ignore the format?
      # my god i am sick of wasting my day on this bullshit
      
      # fix NAs created by locs recorded during daylight savings time  
      locsFormat$DT <- ifelse(!is.na(locsFormat$DT), locsFormat$DT, 
        # by pasting the date
        as.POSIXct(paste(locsFormat$Date, 
        # together with a time that is one hour earlier than the one recorded
        strftime((as.POSIXct(locsFormat$Time, format = "%H:%M:%S") - 1*60*60), format = "%H:%M:%S"), 
            sep = " "), format = "%Y-%m-%d %H:%M:%S"))        
            
      # # fix NAs created by locs recorded during daylight savings time  
      # locsFormat$DT <- ifelse(!is.na(locsFormat$DT), locsFormat$DT, 
      #   as.POSIXct(paste(locsFormat$Date, 
      #     strftime((as.POSIXct(locsFormat$Time, format = "%H:%M:%S") - 1*60*60), format = "%H:%M:%S"),  
      #     sep = " "), format = "%Y-%m-%d %H:%M:%S"))  
      # locsFormat$DT = as.POSIXct(locsFormat$DT, format = "%Y-%m-%d %H:%M:%S", origin = '1970-01-01')  
      
      
      ## KRISTIN YOU LEFT OFF HERE
      ## the above works
      ## the below puts DT back into that numeric mumbo-jumbo and you're having trouble putting it back
      ## purpose of below code is to replace JUST NAs with updated hour.
      
      z <- locsFormat
      z$DT <- ifelse(!is.na(z$DT), z$DT, paste(z$Date, z$Time, sep = " "))
      # expected result: DT stays in correct format and previous NAs are still NAs
      # ok that is not what happens. this already effs up the format. try something simpler.
       z <- locsFormat
      z$DT <- ifelse(!is.na(z$DT), z$DT, NA) 
      # this also effs up the format. something about just transferring the number makes it wonky
      # can i put it back?
      str(z) # shit it's numeric
      z$DT <- as.POSIXct(z$DT) # needs origin
      z$DT <- as.POSIXct(z$DT, origin = "1970-01-01")
      str(z) # oh. it's back. is it correct? -yep . cool.
      

#### suddenly can't connect to access; what gives? ####
      
      z <- odbcConnect("MS Access Database; DBQ = C:/Users/kristin/Documents/MontanaElkCode/mtElkCode/Statewide_Elk_GPS_copy.accdb")
      # this reduced number of errors from 4 to 2, still says not a valid file name
      z <- odbcConnect("MS Access Database; DBQ = C:\\Users\\kristin\\Documents\\MontanaElkCode\\mtElkCode\\test.accdb")
      # ok this returns same error, which actually makes sense bc that file doesn't exist
      list.files(path = "C:/Users/kristin/Documents/MontanaElkCode/mtElkCode")
      # the db does come up in the list of files in the folder
      z <- odbcConnect("MS Access Database; DBQ = C:/Users/kristin/Documents/MontanaElkCode/mtElkCode/Statewide_Elk_GPS_copy.accdb")
      # but copying the file name from that list into the code doens't help
      z <- odbcConnect("MS Access Database; DBQ = C:\\Users\\kristin\\Documents\\MontanaElkCode\\mtElkCode\\Statewide_Elk_GPS_copy.accdb")
      # and neither does changing the slashes used in the filepath
      z <- odbcConnect("MS Access Database; DBQ = C:/Users/kristin/Documents/DatabasesEtc/Statewide/ElkStatewide_Elk_GPS.accdb")
      # and neither does pointing to the file that used to work
      # let's try old code that's always worked and see if have same problem
      # ok, old code does still work, so it's definitely user error...
      
      # closed and reopened to remove stored working directory; rerunning old code without setting wd first
      # ok that works, so it's not an issue of skipping that step at startup
      
      # here's the working code, let's try pasting in filepath i'm using now
     channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                            dbq=C:/Users/kristin/Documents/DatabasesEtc/Statewide/Elk/Statewide_Elk_GPS.accdb")
     channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                            dbq=C:/Users/kristin/Documents/MontanaElkCode/mtElkCode/Statewide_Elk_GPS_copy.accdb")
     # wtf, works fine
     # non-working code is
      channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                            dbq=C:/Users/kristin/Documents/MontanaElkCode/mtElkCode/Statewide_Elk_GPS_copy.accdb")   
      # omfg seriously? only issue was my personal obsession with adding spaces around symbols. christ.
      # well that was a nice way to burn 30 minutes or so.
      
      
      
      
#### starting with summary stuff ####
      
      # use mvmt2dt() to get durations
      
      mvmt2dt(mtop) # nope, need the mvmt/mvmts class
      # hm, so have to use the object that doesn't incl the top identified model?
      test <- mvmt2dt(mref1)
      # error... so can't use the refined model, that seems sketchy
      # oh actually for us this is luckily ok
      # because the refined model only affected a resident parameter
      # which isn't important for this analysis
      test <- mvmt2dt(mBase)
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
      t2$sprStart <- lapply(t, as.character(x), x["str1", "date"]) # n
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

   
      
         
    ### ~~ HERE'S THE WORKING CODE TO COPY ~~ ###
      
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
            
      
      
      
      
    #### BACK AT IT WITH ALL ELK DATA NOW (minus 1 indiv, temporarily) ####      
      

      # get list of movement dates derived from migrant models
      test <- mvmt2dt(mBase)       
      test <- mvmt2dt(mref1) 
      # both give same warnings
      # so the good news is, i can probably use the refined models
      # and the bad news is, this isn't actually working
      # error in chol2inv(object$m$Rmat()) : 'a' must be a numeric matrix
      # error in if (jdts[2] > jdts[3]) { : missing value where TRUE/FALSE needed
      
      # ok that second error seems like it would stem from indivs without mig models
      # so check those as the issue first
      # by removing those indivs and rerunning
          # from model, or have to go back to lt TO REMOVE?
      
      mref2 <- mref1
      str(mref2)
      mref2[[1]] # obj of mvmt class w 3 slots
      mref2[[1]]@data # decimalday, nsd, cut (not sure what cut means, it's FALSE)
      mref2[[1]]@param # lower, start, and upper of each param (gamma, thera, phi, delta, rho, phi2, zeta, kappa)
      mref2[[1]]@models # each actual model, including sum or squares, params, model formula, convergence info
      # i don't see anywhere that the elkYr is stored here, so need to index by elkYrs probably
      # check by comparing the params you outputted earlier against this
      # actually first 
      names(mref2)
      # ah ha, duh.
      mref2["BT10091-2011"] # cooool
      mref2[["151200-2015"]]@param # i don't get it, these don't match my output
      mref2[["151200-2015"]]@models # ohhh those must be averages across all models or something, mig model ones match
      
      which(names(mref2) == "BT10091-2011") # sweet
      mref2$'BT10091-2011'
      
      test <- mref2[-mref2$'BT10091-2011'] # n
      test <- mref2[-1] # y :)
      test <- mref2[-"BT10091-2011"] # n
      test <- mref2[-c(1, 2)] # y
      z <- c(1, 2); rm(test) # y 
      test <- mref2[-z] # y 
      rm(test)
      z <- which(names(mref2) == "BT10091-2011")
      test <- mref2[-z] # y
      rm(z, test)
      z <- which(names(mref2) == "15006-2015" | names(mref2) == "15011-2015")
      test <- mref2[-z] # eff y
      # ok can remove from the model, don't have to go back to lt, sweet
      
      

      
      # identify indivs with no mig model (hard-coded for now unfortunately)
      noMig <- which(
        names(mref1) == "15006-2015" | 
        names(mref2) == "15011-2015" |
        names(mref2) == "15012-2015" |
        names(mref2) == "15015-2015" |
        names(mref2) == "15018-2015")
      
      # remove those indivs from the model & parameter list
      mrefSub <- mref1[-noMig]
      
      # try mvmt2dt again
      test <- mvmt2dt(mrefSub) # ah shit, now it's a list instead of a mvmts
      # ok never mind, have to go back to lt. lame.
      
      test <- lt[-noMig] # n, list
      test <- as.ltraj(lt[-noMig]) # n, ugh, gotta go back to locsModel. laaaaame
      
      
      
      ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##       
      
      locsModelSub <- locsModel %>%
        filter(elkYr != "15006-2015" & 
               elkYr != "15011-2015"  & 
               elkYr != "15012-2015" & 
               elkYr != "15015-2015"  & 
               elkYr != "15018-2015")
      
      
      #### Format data as ltraj object ### 
      
      ltSub <- as.ltraj(
        xy = locsModelSub[,c("utmX", "utmY")], 
        date = locsModelSub$DT,
        id = locsModelSub$elkYr
        )  


    
   #### Identify most parsimonious starting location for NSD (rNSD) ####
      
    
    rlocsSub <- findrloc(ltSub) 
      
       
    
    #### Define initial parameter constraints ####
        
        # expand default duration on summer range to allow up to 8 months (default was 84 days)
        timing <- pEst(u.r = 240)  

      
    
    #### Define base model ####
        
        
        # rNSD with expanded duration parameter and updated rloc 
        mBaseSub <- mvmtClass(ltSub, rloc = rlocsSub$rloc, p.est = timing) 
        
        # print number of convergence issues
        length(which(!fullmvmt(mBaseSub))) # 
    
    
      
    #### figuring out season dates ####
        
          
        # allow up to 8km daily displacement within the same resident range
        uk64 <- pEst(u.k = log(64))
        
        # run updated model
        mref1Sub <- refine(mBaseSub, p.est = uk64)
        
        # print number of convergence issues
        length(which(!fullmvmt(mref1Sub))) # 
        
        
        # try mvmt2dt() on model without indivs missing mig model
        mvmt2dt(mref1Sub)
        # damn, that wasn't the problem. 
        
        # does it fail for the first indiv?
        mvmt2dt(mref1Sub[[1]])
        # it does not. 
        # so next step is to run through each and figure out where it fails
        # may make sense to subset to only animals i classify as migrants,
        # could be an issue of some params not estimated for some indivs
        # but feels like could be some other data issue i haven't uncovered
        
        # oh shit, i just found NAs in my classified behaviors. doh. fix that first.
        
        
    #### NA behavioral classifications ####
        
        length(which(is.na(reclass$Reclass))) # 53?! uuuugh.
        length(which(is.na(reclass$Model))) # not an issue with unclassified indivs from migrateR
        # ok i think these should just stay as their previous classifications
        # and my code somehow didn't incorporate that
        # so to address:
          # 1. look at parameter rules in relation to the parameters of each indiv
          # 2. visual NSD plot inspection (pretty sure they should all look like residents)
          # 3. fix code classifications accordingly
        
        # problem source is that previous version of migrateR
        # estimated delta (dist bt ranges) for residents
        # but this one doesn't. i was using that to reclassify residents.
        # so back to visual plot inspections, oh boy. 
        # OH or i could just use the older version of migrateR?
        # alright first see if you can do it with kappa
        
        # quick personal learning playtime:
        # spit out pdf of nsd plots for only these indivs
        
      elkYrsSub <- reclass %>%
        filter(is.na(Reclass)) %>%
        dplyr::select(elkYr) %>%
        mutate(index = which(is.na(reclass$Reclass)))
      
      names(mref1[elkYrsSub[1,1]])
      which(names(mref1[elkYrsSub[1,1]])) # n
      which(names(mref1[]) == elkYrsSub[1,1]) # y :)
      which(names(mref1[]) == elkYrsSub[2,"elkYr"]) # y
 
        
      #### Define number of plots to create ###
          
        numPlotsSub <- nrow(elkYrsSub)
        myPlotsSub <- vector(numPlotsSub, mode='list')
      
 
      ##### Create a plot for each individual ###
      
        for(i in 1:numPlotsSub) {
          toPlot <- which(names(mref1[]) == elkYrsSub[i,"elkYr"])
          plot(mref1[[toPlot]])
          myPlotsSub[[i]] <- recordPlot()
        }
        graphics.off()
      
     #### Create and store one pdf file of all plots ###
        
        pdf('NSDplotsSub.pdf', onefile = TRUE)
        for(my.plot in myPlotsSub) {
          replayPlot(my.plot)
        }
        graphics.off()        
        
    # nicely done, self. 
        
        plot(mref1[["BROOT1113-2012"]])
        
        
        
    #### haha great, just noticed a potential errant location in one of these plots ####
        
        # 42091−2004
        z <- filter(locsFormat, elkYr == "42091-2004")
    
        zz <- SpatialPointsDataFrame(data.frame("x"=z$Longitude,"y"=z$Latitude), z, proj4string = latlong)


        rgdal::writeOGR(zz, dsn = "../../GIS/Shapefiles/Elk/IndivLocs", layer="errorChecking-42091-2004", driver="ESRI Shapefile",
           overwrite_layer = TRUE)
        
        
        # seems unlikely to be correct, but have no evidence to the contrary
        # it's like 15km from any other location; elk would've had to go that far in abt 4 hours 
        # and then gone right back
        # whatever, deal with this later
        
        
    #### back to the resident delta/gamma fiasco ####
        
        
        # plan: first quickly glance at gamma vals for just residents to see 
            # whether can use them to distinguish like you used delta before
        
            paramsRes <- paramlist[[3]]
            paramsRes$elkYr <- rownames(paramsRes)
            
            paramsResSub <- semi_join(paramsRes, elkYrsSub, by = "elkYr")
            write.csv(paramsResSub, file = "parametersResident.csv", row.names = F)
            
            # used this to verify can use "new" (suspect same as before) gamma parameter
            # the same way we were using delta parameter before to reclassify residents.
            # now just have to figure out how to code that...
        
    
            
    #### back to determining season dates ####
     
                         
      ## issue: mvmt2dt errors ##
            
      z <- mvmt2dt(mref1)
            
      ## step 1: remove indivs that throw warnings and see if they're the only problem
            
      warnings()
      
      # doing it ghetto for efficiency's sake
      probIndivs <- data.frame(elkYr = c("140070-2014", "140380-2014", "140630-2014", "140680-2014",
        "140800-2015", "141070-2014", "141120-2015", "141340-2014", "141490-2015", "15006-2015", 
        "15010-2015", "15011-2015", "15012-2015", "15015-2015", "15018-2015", "15020-2015"))
      
      # and as we already learned i have to go all the way back to locsModel to check whether this works
      
      locsModelSub <- locsModel %>%
        anti_join(probIndivs, by = "elkYr")
      
      
      #### Format data as ltraj object ### 
      
      ltSub <- as.ltraj(
        xy = locsModelSub[,c("utmX", "utmY")], 
        date = locsModelSub$DT,
        id = locsModelSub$elkYr
        )  


    
   #### Identify most parsimonious starting location for NSD (rNSD) ##
      
    
    rlocsSub <- findrloc(ltSub) 
      
       
    
    #### Define initial parameter constraints ###
        
        # expand default duration on summer range to allow up to 8 months (default was 84 days)
        timing <- pEst(u.r = 240)  

        # rNSD with expanded duration parameter and updated rloc 
        mBaseSub <- mvmtClass(ltSub, rloc = rlocsSub$rloc, p.est = timing) 
        
        # print number of convergence issues
        length(which(!fullmvmt(mBaseSub))) # 60
    

        # allow up to 8km daily displacement within the same resident range
        uk64 <- pEst(u.k = log(64))
        
        # run updated model
        mref1Sub <- refine(mBaseSub, p.est = uk64)
        
        # print number of convergence issues
        length(which(!fullmvmt(mref1Sub)))  # 25
        
        
        # try mvmt2dt() on model without indivs that threw warnings
        mvmt2dt(mref1Sub)
        # damn, that wasn't the problem either.       
        
        
        
        
      ## next step: try mvmt2dt with only migrants
        
        

      # doing it ghetto for efficiency's sake
      probIndivs <- reclass %>%
        filter(Model == "migrant") %>%
        dplyr::select(elkYr)
      
      # and as we already learned i have to go all the way back to locsModel to check whether this works
      
      locsModelSub <- locsModel %>%
        semi_join(probIndivs, by = "elkYr")
      
      
      #### Format data as ltraj object ### 
      
      ltSub <- as.ltraj(
        xy = locsModelSub[,c("utmX", "utmY")], 
        date = locsModelSub$DT,
        id = locsModelSub$elkYr
        )  


      # rNSD
    
      rlocsSub <- findrloc(ltSub) 
      
       
    
    #### Define initial parameter constraints ###
        
        # expand default duration on summer range to allow up to 8 months (default was 84 days)
        timing <- pEst(u.r = 240)  

        # rNSD with expanded duration parameter and updated rloc 
        mBaseSub <- mvmtClass(ltSub, rloc = rlocsSub$rloc, p.est = timing) 
        
        # print number of convergence issues
        length(which(!fullmvmt(mBaseSub))) # 40
    

        # allow up to 8km daily displacement within the same resident range
        uk64 <- pEst(u.k = log(64))
        
        # run updated model
        mref1Sub <- refine(mBaseSub, p.est = uk64)
        
        # print number of convergence issues
        length(which(!fullmvmt(mref1Sub)))  # 14
        
        
        # try mvmt2dt() on mod  
        mvmt2dt(mref1Sub) # ooook this works, thank god
        # so issue was just trying to run mvmt2dt for non-migrants. fair enough, they prob had crazy params.
        warnings() # the 50 warnings i can see involve overlapping mvmts; not sure how many of these there are
        
        datesMvmt <- mvmt2dt(mref1Sub) # stored as list
        datesMvmt[[1]]
        datesMvmt[1]
        
        
    #### real quick try it with the ones *I* classified as migrants ####
        

      # 
      migIndivs <- reclass %>%
        filter(Reclass == "migrant") %>%
        dplyr::select(elkYr)
        
      
      locsModelSub1 <- locsModel %>%
        semi_join(migIndivs, by = "elkYr")
      

      ltSub1 <- as.ltraj(
        xy = locsModelSub1[,c("utmX", "utmY")], 
        date = locsModelSub1$DT,
        id = locsModelSub1$elkYr
        )  


      # rNSD
    
      rlocsSub1 <- findrloc(ltSub1) 
      

        # rNSD with expanded duration parameter and updated rloc 
        mBaseSub1 <- mvmtClass(ltSub1, rloc = rlocsSub1$rloc, p.est = timing) 
        
        # print number of convergence issues
        length(which(!fullmvmt(mBaseSub1))) # 24

        
        # run updated model
        mref1Sub1 <- refine(mBaseSub1, p.est = uk64)
        
        # print number of convergence issues
        length(which(!fullmvmt(mref1Sub1)))  # 10
        
        
        # try mvmt2dt() on mod  
        mvmt2dt(mref1Sub1) # eff yeah, and only 39 warnings.
        
        
        
        
    #### extract mvmt dates per individual ####
        
        datesMvmt <- mvmt2dt(mref1Sub1)
        # subset the data for early playtime
        z <- datesMvmt[c(1, 15, 42)]
        str(z[1]) # this is a list that consists of 1 element, a df named with elkYr
        str(z[[1]]) # this is the dataframe itself
        names(z[[1]]) # these are the names of the df columns, NOT the name of that list element
        ?lapply
        lapply(z, names) # cool, gives name of ea element in the list AND names of elements within each element
        # ok, so can you pull dates out -- oh wait
        # you can just index this with i's.
        
        
        # STEPS #
        
        # format each dataframe longways
          # rownames need to be columns (str1, end1, str2, end2)
            # don't necessarily need dday if easier to just keep date; can recreate
          # add column elkYr of dataframe name
        # combine all dataframes
        # calculate durations
        # add distances (from model parameters)
        
        zz <- z[1]
        tidyr::gather(zz) # newp
        str(zz[[1]])
        zL <- gather(zz[[1]])
        View(zL) # oh this gets screwy bc they're in diff formats
        gather(zz[[1]], "date") # n, this makes date the key column
        gather(zz[[1]], date) # samesies
        # oh oops need to add rownames first
        zz$`140040-2015`$param <- rownames(zz$`140040-2015`)
        zz$`140040-2015`$elkYr <- names(zz)
        zz
        zz[[1]] <- zz[[1]][2:4]
        zz
        gather(zz[[1]]) # n
        gather(zz[[1]], key = "param", value = "date") # n --- ohhh
        spread(zz[[1]]) # n
        spread(zz[[1]], "date", "param") # n
        spread(zz[[1]], key = "param", value = "date") # n but closer
        
       
        
         
      ## refine the below... nice start ##
        
        # just pulled one list to start with
        zz <- z[1]
        
        # add columns for elkYr and the rownames (str1, end1, etc)
        zz$`140040-2015`$param <- rownames(zz$`140040-2015`)
        zz$`140040-2015`$elkYr <- names(zz)        
        
        # get rid of dday to make spread() easier (could do this in a pipe)
        zz[[1]] <- zz[[1]][2:4]      
        
        # and make columns and rows do what you want them to
        test <- spread(zz[[1]], key = "param", value = "date") 
        str(test)
        
        
      ## ok, now write it more generically and see if you can do 2
        
        length(z)
        
        datesMig <- as.data.frame(matrix(ncol = 5, nrow = 0))
        colnames(datesMig) <- c("elkYr", "end1", "end2", "str1", "str2")
        
        for (i in 1:length(z)) {
          # dat <- z[[i]] %>%
          #   mutate(param = rownames(dat), elkYr = names(z[i])) %>%
          #   spread(key = "param", value = "date") %>%
          #   bind_rows(datesMig)
          dat <- z[[i]]
          dat$param <- rownames(dat)
          dat$elkYr <- names(z[i])
          dat <- dplyr::select(dat, -dday)
          dat <- spread(dat, key = "param", value = "date")

          datesMig <- bind_rows(datesMig, dat)
          
        }
        
        # motherfuckin a right
        
        
    #### crap, need to programmatically deal with mixed migrants ####
        
        
        # Identify mixed migrants (need to handle specially)
        indivsMix <- reclass %>%
          filter(Reclass == "migrant" & gamma > 900) %>%
          dplyr::select(elkYr)
        
        # Identify individuals classified as migrants
        indivsMig <- reclass %>%
          filter(Reclass == "migrant") %>%
          # but not mixed migrants bc they don't usually have a migrant model
          anti_join(indivsMix, by = "elkYr") %>%
          dplyr::select(elkYr)
        
        # Rerun models using only migrants to allow date extraction
        locsModelMig <- semi_join(locsModel, indivsMig, by = "elkYr")
        ltMig <- as.ltraj(
          xy = locsModelMig[,c("utmX", "utmY")], 
          date = locsModelMig$DT,
          id = locsModelMig$elkYr)
        rlocsMig <- findrloc(ltMig)
        mBaseMig <- mvmtClass(ltMig, rloc = rlocsMig$rloc, p.est = timing) 
        mref1Mig <- refine(mBaseMig, p.est = uk64)

        
        # Extract movement dates from migrant models (warnings here are OK)  
        datesModel <- mvmt2dt(mref1Mig)
        

        # Create dataframe to store formatted dates in
        datesMig <- as.data.frame(matrix(ncol = 5, nrow = 0))
        colnames(datesMig) <- c("elkYr", "end1", "end2", "str1", "str2")
        
        
        # Store formatted movement dates for all migrants
        for (i in 1:length(datesModel)) {
          
          # For each individual
          dat <- datesModel[[i]]
          
          # Make column to store parameter names
          dat$param <- rownames(dat)
          
          # Add column for individual identifier
          dat$elkYr <- names(datesModel[i])
          
          # Remove pesky column that messes things up
          dat <- dplyr::select(dat, -dday)
          
          # Format data appropriately to combine with other indivs
          dat <- spread(dat, key = "param", value = "date")

          # And combine
          datesMig <- bind_rows(datesMig, dat)
          
        }
        
        
        # Add mixed migrants 
                
        

	        
#### trying to subset certain indivs from mvmt2dt so don't have to rerun everything ####
        
        # subset??
        test <- mvmt2dt(mref1Mig[[1]]) # oh. no shit.
        test <- mvmt2dt(mref1Mig[[1]], mod = "mixmig")
        mvmt2dt(mref1Mig[[1:3]]) # error: recursive indexing failed at level 2
        mvmt2dt(mref1Mig[[1:2]]) # error: subscript out of bounds
        
        test <- list()
        for (i in 1:3) {
         test[i] <- mvmt2dt(mref1Mig[[i]]) 
        } # this works but loses elkYr, seems pretty fixable tho
        
        test <- list(length = nrow(elkYrs)) # n
        
        test <- list()
        for (i in c(1, 3, 7)) {
          for (j in 1:3) {
            elk <- elkYrs[j, "elkYr"]
            test[j] <- mvmt2dt(mref1Mig[[i]])
            names(test)[[j]] <- elk
          }
        } # oooooooooh
        
        
        # Identify mixed migrants (need to handle specially)
        indivsMix <- reclass %>%
          filter(Reclass == "migrant" & gamma > 900) %>%
          dplyr::select(elkYr)
        
    
            
### this was so close
        
        
        # Identify individuals classified as migrants
        indivsMig <- reclass %>%
          filter(Reclass == "migrant") %>%
          dplyr::select(elkYr)
        
        # Rerun the models using only migrants to allow date extraction
        locsModelMig <- semi_join(locsModel, indivsMig, by = "elkYr")
        ltMig <- as.ltraj(
          xy = locsModelMig[,c("utmX", "utmY")], 
          date = locsModelMig$DT,
          id = locsModelMig$elkYr)
        rlocsMig <- findrloc(ltMig)
        mBaseMig <- mvmtClass(ltMig, rloc = rlocsMig$rloc, p.est = timing) 
        mref1Mig <- refine(mBaseMig, p.est = uk64)

        
        # Extract movement dates from models (warnings here are OK)  
        datesModel <- mvmt2dt(mref1Mig)
       
        
                
        
        
        # Identify migrants and mixed migrants (need to estimate dates differently for mixed)
        # SOMETHING HERE ISN'T INDEXING THE CORRECT INDIVIDUALS IS ALL
        # PULL FROM ELKYRS OR WHATEVER FED INTO THE MODELS, NOT RECLASS
        indexMig <- which(reclass$Reclass == "migrant" & reclass$gamma < 900 |
            reclass$Reclass == "migrant" & is.na(reclass$gamma))
        indexMix <- which(reclass$Reclass == "migrant" & reclass$gamma > 900)
        
        names(mref1[100])
        elkYrs[100, "elkYr"]
        reclass[100, "elkYr"] # yep that's the issue alright

        
        
     ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
                
        
        # Identify mixed migrants (need to handle specially)
        indivsMix <- reclass %>%
          filter(Reclass == "migrant" & gamma > 900) %>%
          dplyr::select(elkYr)
        indexMix <- which(elkYrs$elkYr %in% indivsMix$elkYr)
        
        # Identify individuals classified as migrants
        indivsMig <- reclass %>%
          filter(Reclass == "migrant") %>%
          # but not mixed migrants bc they don't usually have a migrant model
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
        
        # combine movement dates
        datesModels <- c(datesModelA, datesModelB)
               
        
        
        
  #### figure out how to handle overlapping spr/fall migration dates ####
        
      # first check NSD plots of just those indivs 
      ovr <- datesIndiv[datesIndiv$smrDur <= 0,]
        
 
        numPlotsSub <- nrow(ovr)
        myPlotsSub <- vector(numPlotsSub, mode='list')
      
        for(i in 1:numPlotsSub) {
          toPlot <- which(names(mref1[]) == ovr[i,"elkYr"])
          plot(mref1[[toPlot]])
          myPlotsSub[[i]] <- recordPlot()
        }
        graphics.off()
      

        pdf('NSDplotsDateOvrlap.pdf', onefile = TRUE)
        for(my.plot in myPlotsSub) {
          replayPlot(my.plot)
        }
        graphics.off()                
      
        
      # also check out rho parameters estimated from models
        
      rho <- reclass %>%
        filter(rho < 2 & Reclass == "migrant")
      nrow(rho)
      
      # huh, only 22 according to this, but i have 40 overlappers
      ovrRho <- inner_join(ovr, rho, by = "elkYr")
      # ok, all 22 are included in my list.
      # check the ones i have overlapping dates for that don't have rho = 1
      ovrOnly <- anti_join(ovr, rho, by = "elkYr") %>%
        left_join(reclass, by = "elkYr")
      summary(ovrOnly$rho)

      # let's plot those as well
      
 
        numPlotsSub <- nrow(ovrOnly)
        myPlotsSub <- vector(numPlotsSub, mode='list')
      
        for(i in 1:numPlotsSub) {
          toPlot <- which(names(mref1[]) == ovrOnly[i,"elkYr"])
          plot(mref1[[toPlot]])
          myPlotsSub[[i]] <- recordPlot()
        }
        graphics.off()
      

        pdf('NSDplotsDateOvrlap-GoodRho.pdf', onefile = TRUE)
        for(my.plot in myPlotsSub) {
          replayPlot(my.plot)
        }
        graphics.off()                
        
        
      # moral of the story: 
        # i think using rho makes sense and will alleviate abt half the issues
        # still need to figure out those other ones though
      
        
        
        
  ####  double-checking herd dates make sense before assigning locs to seasons ####
        
        z <- datesHerd %>%
          dplyr::select(
            herdYr, nMig, sprStartMean, sprEndMean, sprDurMean, 
            fallStartMean, fallEndMean, fallDurMean, smrDurMean) %>%
          mutate(
            herd = substr(herdYr, 1, nchar(herdYr)-5),
            sprStartMean = as.POSIXlt(sprStartMean)$yday,
            sprEndMean = as.POSIXlt(sprEndMean)$yday,
            fallStartMean = as.POSIXlt(fallStartMean)$yday,
            fallEndMean = as.POSIXlt(fallEndMean)$yday) %>%
          # fix fall return dates in january
          # otherwise averaging dates could make fall migration end before it starts
          mutate(fallEndMean = ifelse(fallEndMean < 20, fallEndMean + 365, fallEndMean)) %>%
          group_by(herd) %>%
          summarise(
            nElk = sum(nMig),
            nYrs = n(),
            sprStrt = mean(sprStartMean),
            sprEnd = mean(sprEndMean),
            fallStrt = mean(fallStartMean),
            fallEnd = mean(fallEndMean),
            sprDur = round(mean(sprDurMean)),
            fallDur = round(mean(fallDurMean)),
            smrDur = round(mean(smrDurMean)),
            winDur = I(365-sprDur-fallDur-smrDur)) %>%
          mutate(
            sprStrt = format(strptime(sprStrt, format = "%j"), format = "%m-%d"),
            sprEnd = format(strptime(sprEnd, format = "%j"), format = "%m-%d"),
            fallStrt = format(strptime(fallStrt, format = "%j"), format = "%m-%d"),
            fallEnd = format(strptime(fallEnd, format = "%j"), format = "%m-%d"))            
        
        
        str(z)
        test <- z$sprStartMean
        test2 <- format(strptime(test, format = "%j"), format = "%m-%d")
        test2 # cool
        ?strptime
        
        ## to make these averages correct
        ## you need to make herd averages when there's only one indiv
        ## be == that indiv's dates, not NA.
        ## OH you shouldn't have NAs. there's just one from the mixed migrant
        ## need to extract duration and distance from mig model for that one
        
        mref1$"SC11074-2012"
        str(mref1$"SC11074-2012")
        mref1$"SC10074-2012"$mixmig #n
        mref1$"SC10074-2012"@models$mixmig #n
        mref1$"SC10074-2012"@models #n
        mref1$"SC10074-2012"@"models"
        mref1$"SC10074-2012"[[2]]
        
        # get out of the console and get your shit together kristin
        
        ## need to extract parameters from mixmig model for the mixmig
        
        
        
        
        
  #### STILL date-time issues... ####
        
      
      ## oh lord, just realized something's still wrong with the random selection code
      ## it's only using nighttime locations
      ## i thought you fixed this already...
      
      # ok. challenge is that times are being read in from access as dates
      # i thought that wasn't happening before, so maybe you effed up the time column 
      # when you "fixed" those missing locations?
      # alright, so step1 is just read in an older version from before you messed with it
        
    
      # replace the file path after "dbq=" with yours (sorry, i failed to do it programmatically)
      channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
        dbq=C:/Users/kristin/Documents/DatabasesEtc/Statewide/Elk/Statewide_Elk_GPS_MissingBROOTtimes.accdb")
      test <- sqlQuery(channel, paste("select * from ElkGPS"))
      odbcCloseAll()        
      
      # mmmyep i screwed it up. lovely.
      # all good though just need to reformat that column properly.
      
      # ok wtf, it's still formatted as data type Date/Time and format Short Time (just like the older version)
          
      # opened and just re-clicked the same formatting options in the column, doubt it worked but check
      channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
        dbq=C:/Users/kristin/Documents/MontanaElkCode/mtElkCode/Statewide_Elk_GPS.accdb")
      test2 <- sqlQuery(channel, paste("select * from ElkGPS"))      
      odbcCloseAll()                 
      # yeah no.
      
      # step 2: try re-importing and formatting the time column as such
      # oh oh use that xls kelly sent you and see how they had time formatted there too
      # they had time as general; imade mine that way by pasting using "formulas and number formatting"
      
      
      channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
        dbq=C:/Users/kristin/Documents/MontanaElkCode/mtElkCode/Statewide_Elk_GPS_plzwork.accdb")
      test2 <- sqlQuery(channel, paste("select * from ElkGPS"))
      odbcCloseAll()      
      # motherfucker
      # close out and retry; something's screwy (can't open or delete that db, for one)
      
      
      channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
        dbq=C:/Users/kristin/Documents/MontanaElkCode/mtElkCode/Statewide_Elk_GPS_plzwork.accdb")
      test <- sqlQuery(channel, paste("select * from ElkGPS"))
      beepr::beep()
      odbcCloseAll()      
      # motherfucker      
      
      # realized i actually screwed it up in the step before,
      # when i had to replace those missing times. 
      # started over from that step, just imported those times
      
      channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
        dbq=C:/Users/kristin/Documents/MontanaElkCode/mtElkCode/Statewide_Elk_GPS_copy.accdb")
      test <- sqlQuery(channel, paste("select * from ElkGPS"))
      beepr::beep()
      odbcCloseAll() 
      # ok wtf. worked after adding the BROOT1131 times but not after adding the BROOT0015 times
      #             
      
      # went back through and redid the excel formatting and import into access for both elk
      # i think the times weren't actually formatted as general before.
      
      channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
        dbq=C:/Users/kristin/Documents/MontanaElkCode/mtElkCode/Statewide_Elk_GPS_ihatethis.accdb")
      test <- sqlQuery(channel, paste("select * from ElkGPS"))
      beepr::beep()
      odbcCloseAll() 
      #        
      
      
    #### back to dates and seasons and such ####
      
      
  ## this code did work but it turns out you can't use the mixed migrant mvmt dates bc they make no sense
      
    #### Determine spring and fall movement dates for migrants  ###
        
        
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
        
        
    #### preserving old code in case you eff it up and need to start over [um kristin this is what git is for] ####


        
        # Summarize the above per herd        
        datesHerdYr <- datesMigrants %>%
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
            distSd = round(sd(dist), 2))        
        
        
        
    #### Making herd-averaged dates be in the right year ####
        
        
      z <- allDatIndiv[1:3,c("elkYr", "sprStartMean")]  %>%
          mutate(test = paste(sprStartMean, substr(elkYr, nchar(elkYr)-3, nchar(elkYr)), sep = "-"))
      str(z)
      z$test2 <- as.Date(z$test) #n
      z$test2 <- as.Date(z$test, format = "%m-%d-%Y")
      str(z) # ..k not sure why i couldn't do that yesterday...
      
      # was it an in-pipe issue?
      z <- allDatIndiv[1:3,c("elkYr", "sprStartMean")]  %>%
          mutate(test = as.Date(paste(sprStartMean, substr(elkYr, nchar(elkYr)-3, nchar(elkYr)), sep = "-")), format = "%m-%d-%Y")      
      # oh. yes it was. can't wrap it in as.Date for some reason, but can still do in pipe?
      
      z <- allDatIndiv[1:3,c("elkYr", "sprStartMean")]  %>%
          mutate(test = paste(sprStartMean, substr(elkYr, nchar(elkYr)-3, nchar(elkYr)), sep = "-"),
            test2 = as.Date(test, format = "%m-%d-%Y"))
      str(z) #y
      
      # re-mutate same column twice?
      z <- allDatIndiv[1:3,c("elkYr", "sprStartMean")]  %>%
          mutate(test = paste(sprStartMean, substr(elkYr, nchar(elkYr)-3, nchar(elkYr)), sep = "-"),
            test = as.Date(test, format = "%m-%d-%Y"))      
      str(z) # y. cool do that.
       
      
      
  #### labeling season for each location ####
      
      # just make this be locsFormat after you get it working
      z <- sample_n(locsFormat, 20, replace=F) %>%
        # add season dates for each individual
        left_join(datesIndivs, by = c("elkYr", "Herd")) 
      # oh right, shit, now you have to set dates for indivs not used in behav analysis.
      # do that first? nah, do this quick while you're in the middle of it, then go back
      
      z <- sample_n(locsModel, 20, replace=F) %>%
        left_join(datesIndivs, by = c("elkYr", "Herd")) %>%
        mutate(Season = ifelse(
          between(Date, win1Start, win1End), "Winter", "YTRAS")) # n
      
      z <- sample_n(locsModel, 20, replace=F) %>%
        left_join(datesIndivs, by = c("elkYr", "Herd")) %>%
        mutate(Season = ifelse(Date >= win1Start & Date <= win1End, "Winter",
          ifelse(Date >= sprStart & Date <= sprEnd, "Spring",
            ifelse(Date >= smrStart & Date <= smrEnd, "Summer", "Fall")))) %>%
        dplyr::select(elkYr, herdYr, Date, Season, win1Start, win1End, sprStart, 
          sprEnd, smrStart, smrEnd, fallStart, fallEnd, win2Start) # y :)
      
      
      
  #### labeling season for non-model elk #### 
      
      # current thought for best plan
      # remove modeled elk from locsFormat, then figure out which of the ones left over had >20 locs per season
        # will req adding elkYr, and using herd average dates
      # add those elk to indivDates and use same pipe to id dates, add column for behavAnalysis 0/1
        # this is probably better, then can subset whatever from that one master
      
      # this works, but need to expand to incl all elk
        
        # identify season for locs recorded only by elk used in behavioral analysis
        locsSeasons <- locsModel %>%
          left_join(datesIndivsMod, by = c("elkYr", "Herd")) %>%
          mutate(Season = ifelse(Date >= win1Start & Date <= win1End, "Winter",
            ifelse(Date >= sprStart & Date <= sprEnd, "Spring",
              ifelse(Date >= smrStart & Date <= smrEnd, "Summer", "Fall")))) %>%
          dplyr::select(elkYr, herdYr, Date, Season, win1Start, win1End, sprStart, 
            sprEnd, smrStart, smrEnd, fallStart, fallEnd, win2Start)
      
      
    # this also works, but now you want it to be for all elk (start with locsFormat)
      # and add an identifier for whether incld in behav analysis or not
        # Define seasons for all individuals
        datesIndivsMod <- reclass %>%
          #### ~ KRISTIN YOU MAY NEED TO DELETE THIS ~ ####
          # remove the single (non-migratory) elk from Greycliff herd #
          filter(elkYr != "BRUC15042-2016") %>%
          # start with behavior classifications
          rename(behav = Reclass) %>%
          # add herd and herd-year
          left_join(elkYrHerd, by = "elkYr") %>%
          # add herd average dates (to define seasons for non-migrants)
          left_join(datesHerd, by = "Herd") %>%
          # add individuals' dates if migrated
          left_join(datesMigrants, by = "elkYr") %>%
          # remove and fix duplicated columns from joins
          dplyr::select(-c(Herd.y, herdYr.y)) %>%
          rename(Herd = Herd.x, herdYr = herdYr.x) %>%
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
          # remove extraneous columns
          dplyr::select(elkYr, behav, Herd, herdYr, 
            win1Start, win1End, sprStart, sprEnd, smrStart, smrEnd, fallStart, fallEnd, win2Start)      
        
      
    
      
      
      
    # tweaking this to be for all elk 
      # and add an identifier for whether incld in behav analysis or not
        
        # Define seasons for all individuals
        datesIndivs <- reclass %>%
          # start with behavior classifications
          rename(behav = Reclass) %>%
          # add herd and herd-year
          left_join(elkYrHerd, by = "elkYr") %>%   
          # add identifier for whether elk was included in behavior analysis
          mutate(inBehavAnalysis == 1) %>%
          # add elk that weren't included in behavior analysis
          bind_rows(elkYrHerdNoBehav) %>%
          left_join(datesHerd, by = "Herd") %>%
          # and indicate that they weren't included
          mutate(inBehavAnalysis = ifelse(inBehavAnalysis == 1, 1, 0)) %>%
          # add individuals' dates if migrated
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
          # remove extraneous columns
          dplyr::select(elkYr, behav, Herd, herdYr, inBehavAnalysis,
            win1Start, win1End, sprStart, sprEnd, smrStart, smrEnd, fallStart, fallEnd, win2Start) %>%
          #### ~ KRISTIN IF YOU DON'T DELETE THIS, MOVE IT TO THE START OF THE CODE ~ ###
          # remove the single (non-migratory) elk from Greycliff herd #
          filter(Herd != "Greycliff")
        

        # identify season for all recorded collar locations
        locsSeasons <- locsFormat %>%
          left_join(datesIndivs, by = c("elkYr", "Herd")) %>%
          #### ~ KRISTIN IF YOU DON'T DELETE THIS, MOVE IT TO THE START OF THE CODE ~ ###
          # remove the single (non-migratory) elk from Greycliff herd #
          filter(Herd != "Greycliff") %>%        
          mutate(Season = ifelse(Date >= win1Start & Date <= win1End, "Winter",
            ifelse(Date >= sprStart & Date <= sprEnd, "Spring",
              ifelse(Date >= smrStart & Date <= smrEnd, "Summer", "Fall")))) %>%
          group_by(elkYr, Season) %>%
          filter(n() >= 20) %>%
          ungroup() %>%
          dplyr::select(elkYr, herdYr, Herd, AnimalID, CollarID, inBehavAnalysis,
            Date, Time, Latitude, Longitude, Season, 
            win1Start, win1End, sprStart, sprEnd, smrStart, smrEnd, fallStart, fallEnd, win2Start) %>%
          droplevels()
        
        
        
#### Defining winter & summer per indiv based on shortest timeframe ####
        
    
    # determine shortest winter timeframe
    datesIndivs %>% mutate(winDur = win1End-win1Start) %>% summarise(min(winDur))
        
        
    ## current challenge: come indivs have negative winter durations
    ## because they changes their migration date the subsequent year
    ## potential solutions:
      ## calculate -- oh yeah, duh, just calc from start date (1st loc) to winEnd.
        
    ## summer
        
         
         # determine summer timeframe
         smr <- datesIndivs %>%
           mutate(smrDur = as.numeric(smrEnd-smrStart)) %>%
           dplyr::select(elkYr, behav, Herd, herdYr, smrDur, smrStart, smrEnd) %>%
           distinct()
         min(smr$smrDur) # shit, got negatives here of course...from overlappers
         
         
         # what's min herd timeframe?
         min(datesHerd$smrDurMean) # 32, cool.
         
         # what's min migrant timeframe?
         min(datesMigrants$smrDr) # 1. oh duh, those are those overlappers. 
 
         
         # some don't have 5 relocs??
        z <- viLocs %>%
          group_by(elkYr, seasonVI) %>%
          filter(n()<5)
        # it's the BROOT whose collar sucked during summer
        
        View(viLocs[viLocs$elkYr == "BROOT0021-2011",])
        
        
        
#### PREserving code i'm changing bc github has failed me in rstudio ####
        
        
        

        
        # Per herd
        dateDistHerd <- dateDistHerdYr %>%
          # only keep relevant columns
          dplyr::select(
            herdYr, nMig, nIndivs, sprStartMean, sprEndMean, sprDurMean, 
            fallStartMean, fallEndMean, fallDurMean, smrDurMean) %>%
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
          # average dates as weighted mean (weighted by number of migrants in each yr)
          summarise(
            nElk = sum(nMig),
            nYrs = n(),
            sprStartMean = weighted.mean(sprStartMean, nMig),
            sprEndMean = weighted.mean(sprEndMean, nMig),
            fallStartMean = weighted.mean(fallStartMean, nMig),
            fallEndMean = weighted.mean(fallEndMean, nMig),
            sprDurMean = round(weighted.mean(sprDurMean, nMig)),
            fallDurMean = round(weighted.mean(fallDurMean, nMig)),
            smrDurMean = round(weighted.mean(smrDurMean, nMig)),
            winDurMean = I(365-sprDurMean-fallDurMean-smrDurMean)) %>%
          mutate(
            sprStartMean = format(strptime(sprStartMean, format = "%j"), format = "%m-%d"),
            sprEndMean = format(strptime(sprEndMean, format = "%j"), format = "%m-%d"),
            fallStartMean = format(strptime(fallStartMean, format = "%j"), format = "%m-%d"),
            fallEndMean = format(strptime(fallEndMean, format = "%j"), format = "%m-%d")) %>%
          # add herd-years for each herd
          left_join(dplyr::select(elkYrHerd, c(Herd, herdYr)), by = "Herd") %>%
          # add correct year of herd-year to averaged dates
          mutate(
            sprStartHerdYr = as.Date(
              paste(substr(herdYr, nchar(herdYr)-3, nchar(herdYr)), sprStartMean, sep = "-"),
              format = "%Y-%m-%d"),
            sprEndHerdYr = as.Date(
              paste(substr(herdYr, nchar(herdYr)-3, nchar(herdYr)), sprEndMean, sep = "-"),
              format = "%Y-%m-%d"),
            fallStartHerdYr = as.Date(
              paste(substr(herdYr, nchar(herdYr)-3, nchar(herdYr)), fallStartMean, sep = "-"),
              format = "%Y-%m-%d"),
            fallEndHerdYr = as.Date(
              paste(substr(herdYr, nchar(herdYr)-3, nchar(herdYr)), fallEndMean, sep = "-"),
              format = "%Y-%m-%d")) %>%
          # remove duplicate rows
          distinct()
         # export
         write.csv(dateDistHerd, "herdDatesDist.csv", row.names = F)       
 
         
         
#### Investigating migrants with large VI ####
         
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
        mutate(seasonVI = "Winter")          
     viLocsWin[viLocsWin$elkYr == "22071-2002", "day1"]
     # ok, the reason this "migrant" has like 50% VI is because
     # it moved almost immediately after capture
     # NSD started with a location down on its winter range; VI used lots of locations after it moved
     
     # ok and double-checked the others; they also make sense

              