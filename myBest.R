best <- function(state, outcome) {

        ## Read the data
        ## If no colClasses set, "out of range" error occurs

        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

        if (state %in% data$State & outcome %in% c("heart failure",
                "heart attack", "pneumonia")) {

                ## Select appropriate data
                if (outcome == "heart attack") {

                        ## This selects column for: State, Hospital.Name, and
                        ## Heart attack outcome
                        a <- data[,c(2,7,11)]
                }

                if (outcome == "heart failure") {
                        a <- data[,c(2,7,17)]
                }

                if (outcome == "pneumonia") {
                        data[,c(2,7,23)]
                }

        ## This sorts the data based on the state
        b <- split(a, a$State)

        ## This selects only the requested state by the user
        c <- b[[state]]

        ## Takes only Mortality data with Hospital.Name
        c <- c[,c(1,3)]

        ## Change colNames
        colnames(c) <- cbind("Hospital.Name", outcome)

        ## Changes class of outcome column to numeric values
        ## Will produce error message:
        ##      NAs introduced by coercion
        c[,2] <- as.numeric(c[,2])

        ## Return Hospital.Name and outcome value for minimum outcome value
        d <- c[which(c[,2]==min(c[,2], na.rm=TRUE)),]

        ## Returns only the name of the hospitals
        best_hospitals <- d$Hospital.Name

        ## Sorts to find first in the list of best hospitals if more than one
        best_hospital <- sort(best_hospitals)[1]

        }

        else if (state %in% data$State == FALSE) {stop("invalid state")}

        else if (outcome %in% c("heart failure", "heart attack", "pneumonia") == FALSE) {
                stop("invalid outcome")
        }

        best_hospital
}
