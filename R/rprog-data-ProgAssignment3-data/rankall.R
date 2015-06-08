rankall <- function(outcome, num = "best") {
        ## Read outcome data
        outCare <- read.csv("outcome-of-care-measures.csv", 
                            colClasses = "character")
        
        oc <- NULL
        outTitle <- names(outCare)
        if (outcome == "heart attack") oc <- outTitle[11]
        if (outcome == "heart failure") oc <- outTitle[17]
        if (outcome == "pneumonia") oc <- outTitle[23]
        if (is.null(oc)) {
                msg1 <- sprintf (" Outcome %s does not exist.\n", outcome)
                msg2 <- sprintf ("List of outcomes supported are \n")
                msg3 <- sprintf ("heart attack, heart failure, & pneumonia \n")
                return(cat(msg1, msg2, msg3))
        }
        
        ## Return hospital names with the given rank
        ## 30-day death rate
        allOutCare <- outCare[!is.na(suppressWarnings (
                as.numeric(outCare[,oc]))), ]
        sortedOrd = allOutCare[order (allOutCare$State, 
                                      as.numeric(allOutCare[,oc]),
                                      allOutCare$Hospital.Name),]
        sortdf <- data.frame(State = sortedOrd$State,
                             Hospital.Name = sortedOrd$Hospital.Name, 
                             Rate = sortedOrd[,oc],
                             Rank = 1:length(sortedOrd$Hospital.Name))
        stRnk <- NULL
        for (i in 1:length(table(sortdf$State))) {
                stRnk <- c(stRnk, 1:table(sortdf$State)[i])
        }
        sortdf$Rank <- stRnk
        
        rnk <- as.integer(num)
        if (num == "best") rnk <- as.integer(1)
        if (num == "worst") rnk <- as.integer(length(sortedOrd$Hospital.Name))
        if (rnk > as.integer(length(sortedOrd$Hospital.Name))) {
                return ("Ranking not available")
        }
        if (!is.integer(rnk)) return ("Ranking is not an integer")
        if (num == "worst") {
                index <- c(tail(which(sortdf$Rank == 1) - 1, -1), 
                           length(sortedOrd$Hospital.Name))
                part <- data.frame(hospital = sortdf[index,]['Hospital.Name'],
                                   State = sortdf[index,]['State'])
        } else {
                part <- data.frame(hospital = sortdf[sortdf$Rank == rnk,]['Hospital.Name'],
                                   State = sortdf[sortdf$Rank == rnk,]['State'])
        }
        comp <- data.frame (State = sort(unique(outCare$State)))                      
        comp <- merge(comp, part, all.x=TRUE)
        colnames(comp) <- c('state', 'hospital')
        comp
        
}