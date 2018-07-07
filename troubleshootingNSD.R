



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
      
        
      ## Fix the problem for those individuals

        # when i ran this, the issue was indivs who had a big gap between
        # the date of their first loc and all subsequent locs -
        # so below code removes the first loc for each indiv who had an error
        
        # identify the rows you want to remove (1st loc for each indiv)
        rm <- locsModel %>%
          filter(elkYr %in% as.character(errorsRloc$elkYr)) %>%
          group_by(elkYr) %>%
          filter(DT == min(DT)) %>%
          ungroup()
        
        # remove those rows from the full dataframe (does nothing if no errors)
        locsModel <- locsModel %>%
          anti_join(rm)

        # rerun "Identify individuals" section above to verify no more errors, then proceed
        # note if you still get errors and have to do more work,
        # you may need to recreate locsModel from the original code 
        # (in case what you just did changed it in an undesirable way)
      
