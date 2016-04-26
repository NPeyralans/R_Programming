##Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
##outcome name. The function reads the outcome-of-care-measures.csv le and returns a character vector
##with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specied outcome
##in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
##e one of \heart attack", \heart failure", or \pneumonia". Hospitals that do not have data on a particular
##outcome should be excluded from the set of hospitals when deciding the rankings.


best <- function(state, outcome) {

        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = 'character')

        ## Check state is valid
        if (!any(state == data$State)){
                stop('invalid state')
        }

        ##Select appropriate column and verify it is valid input
        if (outcome == "heart attack") {
                requestedColumn <- 11
        }

        else if (outcome == "heart failure") {
                requestedColumn <- 17
        }

        else if (outcome == "pneumonia") {
                requestedColumn <- 23
        }

        else {
                stop("invalid outcome")
        }

        state_data <- data[data$State == state, ]
        state_data[, requestedColumn] <- as.numeric(x=state_data[, requestedColumn])

        ## Take out non-information
        state_data <- state_data[complete.cases(state_data), ]

        ## Return hospital name in that state with lowest 30-day death
        return_names <- state_data[(state_data[, requestedColumn] == min(state_data[, requestedColumn])), ]$Hospital.Name
        sort(return_names)[1]

}
