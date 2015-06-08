
rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        outCare <- read.csv("outcome-of-care-measures.csv", 
                            colClasses = "character")
        
        ## Check that state and outcome are valid
        if (!is.element (state, outCare$State)) {
                msg <- sprintf ("State %s does not exist. Please retry", state)
                message(msg)
                stop ("invalid state")
        }
        
        oc <- NULL
        outTitle <- names(outCare)
        if (outcome == "heart attack") oc <- outTitle[11]
        if (outcome == "heart failure") oc <- outTitle[17]
        if (outcome == "pneumonia") oc <- outTitle[23]
        if (is.null(oc)) {
                msg1 <- sprintf (" Outcome %s does not exist.\n", outcome)
                msg2 <- sprintf ("List of outcomes supported are \n")
                msg3 <- sprintf ("heart attack, heart failure, & pneumonia \n")
                message(cat(msg1, msg2, msg3))
                stop ("invalid outcome")
        }
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        stOutCare <- outCare[outCare$State == state & 
                                     !is.na(suppressWarnings (
                                             as.numeric(outCare[,oc]))), ]
        sortedOrd = stOutCare[order (as.numeric(stOutCare[,oc]),
                                     stOutCare$Hospital.Name),]
        sortdf <- data.frame(Hospital.Name = as.character(sortedOrd$Hospital.Name), 
                             Rate = as.numeric(sortedOrd[,oc]), 
                             Rank = 1:length(sortedOrd$Hospital.Name))
        
        rnk <- as.integer(num)
        if (num == "best") rnk <- as.integer(1)
        if (num == "worst") rnk <- as.integer(length(sortedOrd$Hospital.Name))
        if (rnk > as.integer(length(sortedOrd$Hospital.Name))) {
                return (NA)
        }
        if (!is.integer(rnk)) return ("Ranking is not an integer")
        as.character(unlist(sortdf[sortdf$Rank == rnk,]['Hospital.Name']))
}
