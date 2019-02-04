## 3. RANKING HOSPITALS BY OUTCOME IN A STATE

rankhospital <- function(state, outcome, rank = "best"){
        
        # read in data from outcome-of-care-measures.csv file
        
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        # match fields with column numbers
        
        fd   <- as.data.frame(cbind(data[, 2],  # hospital
                                    data[, 7],  # state
                                    data[, 11],  # heart attack
                                    data[, 17],  # heart failure
                                    data[, 23]), # pneumonia
                              stringsAsFactors = FALSE)
        colnames(fd) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
        
        # confirm that state and outcome are valid
        
        if (!state %in% fd[, "state"]) {
                
                # display error message if state not valid
                stop('invalid state')
                
        } else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
                
                # display error message if outcome not valid
                stop('invalid outcome')
                
        } else if (is.numeric(rank)) {
                s <- which(fd[, "state"] == state)
                h <- fd[s, ]                     # extracting dataframe for the called state
                h[, eval(outcome)] <- as.numeric(h[, eval(outcome)])
                h <- h[order(h[, eval(outcome)], h[, "hospital"]), ]
                output <- h[, "hospital"][rank]
        } else if (!is.numeric(rank)){
                
                # if selected rank is the best hospital in the state
                if (rank == "best") {
                        output <- best(state, outcome)
                        
                # if selected rnak is the worst hospital in the state        
                } else if (rank == "worst") {
                        s <- which(fd[, "state"] == state)
                        h <- fd[s, ]    
                        h[, eval(outcome)] <- as.numeric(h[, eval(outcome)])
                        h <- h[order(h[, eval(outcome)], h[, "hospital"], decreasing = TRUE), ]
                        output <- h[, "hospital"][1]
                } else {
                        
                        # display error message if selected rank is not valid
                        stop('invalid rank')
                }
        }
        return(output)
}