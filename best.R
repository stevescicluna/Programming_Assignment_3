## 2. FINDING THE BEST HOSPITAL IN A STATE - TO BE SELECTED

best <- function(state, outcome) {
        
        # read in data from "outcome-of-care-measures.csv" file
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        # confirm that state and outcome are valid
        s = state %in% data$State
        outcomes <- c("heart attack", "heart failure", "pneumonia")
        o = outcome %in% outcomes
        # display error message if state is not valid
        if (s == FALSE){
                stop("Invalid State")
        }
        # display error message if outcome is not valid
        if (o == FALSE){
                stop("Invalid Outcome")
        } 
        
        # display hospital name in that state with lowest 30-day death rate for the selected outcome
        if (outcome == "heart attack"){
                outcomeData = data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
        }
        if (outcome == "heart failure"){
                outcomeData = data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
        }
        if (outcome == "pneumonia"){
                outcomeData = data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
        }
        sdata <- data.frame(data$State, data$Hospital.Name, as.numeric(outcomeData))
        # using as.numeric on `outcomeData` so this does not become class
        z <- subset(sdata, data$State == state)      # Gather rows with the desired state
        ## rate
        
        hname <- c()
        for (i in 1:length(z)){
                zz <- z[i] == min((z[,3]), na.rm = TRUE) #compate the oucome data to the minimum
        }
        result <- data.frame(z[,2], zz)
        fresult <- subset(result, result[,2] == TRUE)
        afresult <- fresult[order(fresult$z...2.),] #Alphabetize results data frame
        print(afresult[1,1])
}
