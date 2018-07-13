



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

        
        
        
        
  #### If you get errors trying to run the actual models (from mvmtClass()) ####
        
        
      ## Run the below to identify problem individuals and their associated errors
        
      ## These errors usually reveal issues with the recorded GPS locations
      ## that prevent models from being estimated.
        
      ## (Note: unlike these errors, warnings from mvmtClass() are generally OK.
      ##  They indicate when a model doesn't converge for an individual,
      ##  which is typically just because that model can't be tortured into fitting
      ##  the data - e.g. trying to fit a migrant model to a resident who goes nowhere)
        

        # create dataframe to store error messages in
        errors <- data.frame(elkYr = unique(locsModel$elkYr), Err = NA)
        
        # for each individual
        for(i in 1:nrow(rlocs)) {
          
          # subset its locations
          ilocs <- droplevels(semi_join(locsModel, rlocs[i,], by = c("elkYr" = "burst")))
          
          # make it ltraj
          ilt <- as.ltraj(xy = ilocs[,c("utmX", "utmY")], date = ilocs$DT, id = ilocs$elkYr)
          
          # try the model and store error message if any
          tryCatch(mvmtClass(ilt, p.est = uk64, rloc = rlocs[i,"rloc"]), error = function(e) {
                  errors[i,"Err"] <<- conditionMessage(e)
                  NULL
              })
        }
        
        # individuals and the errors they had
        errors <- errors[!is.na(errors$Err),]
        View(errors)


     
