



   #### If you get errors from findrloc() ####
      

      ## Run the below to identify individuals for whom rloc didn't work
      ## Errors often reveal issues with earliest recorded locations
      ## (e.g., several missing locations soon after the starting location)

      
        # create dataframe to store error messages in
        errorsRloc <- data.frame(elkYr = unique(locsModel$elkYr), Err = NA)
        
        # for each individual,
        for(i in 1:nrow(errorsRloc)) {
          
          # subset its locations
          iLocs <- filter(locsModel, elkYr == errorsRloc[i,"elkYr"])
          
          # make them ltraj format
          iLt <- as.ltraj(xy = iLocs[,c("utmX", "utmY")], date = iLocs$DT, id = iLocs$elkYr)
          
          # try finding rloc; store error message if any
          tryCatch(findrloc(iLt), error = function(e) {
            errorsRloc[i,"Err"] <<- conditionMessage(e)
            NULL
          })
        }

      
        # individuals and the errors they had
        errorsRloc <- errorsRloc[!is.na(errorsRloc$Err),]
        View(errorsRloc)
        beep("complete")
      
        
