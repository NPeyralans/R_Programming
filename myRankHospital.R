rankhospital <- function(state, outcome, num='best') {

        ## Verify that the num parameter is either 'best' or 'worst'
        if (is.character(num)) {
                if (num %in% c('best', 'worst') == FALSE) {
                        stop('Invalid Rank')
                }
        }


        ## Read data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")


        ## Make sure all parameters are appropriate
        if (state %in% data$State == FALSE) { stop("invalid state")}

        if (outcome %in% c("heart failure", "heart attack", "pneumonia") == FALSE) }
                stop("invalid outcome")
                }


        if (state %in% data$State & outcome %in% c("heart failure", "heart attack", "pneumonia")){

        ## If parameters are correct

                ## Assign requested data to "a"
                if (outcome == "heart attack") {
                        a <- data[,c(2,7,11)]
                }

                if (outcome == "heart failure") {
                        a <- data[,c(2,7,17)]
                }

                if (outcome == "pneumonia") {
                        a <- data[,c(2,7,23)]
                }

        ## Sort data by state
        b <- split(a,a$State)

        ## Select only requested state
        c <- b[[state]]

        ## Only needed columns
        c <- c[,c(1,3)]

        ## Add colnames to c data frame
        colnames(c) <- cbind('Hospital.Name', outcome)

        ## Convert "character" data from spreadsheet to "numeric"
        c[,2] <- as.numeric(c[,2])

        ## Now only the required data is to be found in "c"

        if (num='best'){

                ## "d" outputs the lowest mortality rate while removind nas
                d <- c[which(c[,2] == min(c[,2], na.rm=TRUE)),]

                ## In case there are more than one hospital with that rate:
                hospitals <- d$Hospital.name

                ## Select first in alphabetical order
                hospital <- sort(hospital)[1]
        }

        if (num == 'worst') {

                d <- c[which(c[,2]==max(c[,2], na.rm==TRUE)),]

                hospitals <- d$Hospital.Name
                hospital <- sort(hospitals)[1]

        }


        ## If num parameter is a numeric value, check and make sure it is
        ## within the domain of the requested data set from above.
        if (is.numeric(num) & num < length(c$Hospital.Name)) {

                ## Remove all na values from dataset
                l <- c[!is.na(c[,2]),]

                ## Sorts by mortality rates, places in order, and removes duplicates
                ll <- unique(sort(l[,2]))

                
                m <- c()

                for (i in l:length(ll)) {
                        temp <- which(1[,2]==ll[i])
                        m <- rbind(m,l[temp,])
                }



        }



        }
}
