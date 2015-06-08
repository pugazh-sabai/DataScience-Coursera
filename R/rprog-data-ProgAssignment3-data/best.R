best <- function(state, outcome) {
        
        ## Read outcome data
        outCare <- read.csv("outcome-of-care-measures.csv", 
                            colClasses = "character")
        
        ## Check that state and outcome are valid
        if (!is.element (state, outCare$State)) {
               stop("invalid state")
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
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        bestRateIndex <- suppressWarnings (which.min(outCare[outCare$State 
                                                             == state, oc]))
        bestRate <- as.character(outCare[outCare$State == state,
                                         oc][bestRateIndex])
        hosp.Name <- outCare[outCare$State == state & 
                                     outCare[oc] == bestRate,]$Hospital.Name
        hosp.Name
}
