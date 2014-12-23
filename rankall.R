rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
 
#    state_data <- data[data$State == state, ]
#    if (nrow(state_data) < 1 ) {
#        stop("invalid state")
#    }

    if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
        stop("invalid outcome")
    }
    
    if (outcome == "heart attack"){
        outcomeID = 11
    } else if (outcome == "heart failure"){
        outcomeID = 17
    } else if (outcome == "pneumonia"){
        outcomeID = 23
    }
    
    sc_data <- data[, c(2, 7, outcomeID)]
    sc_data <- sc_data[sc_data[, 3] != "Not Available", ]
    sc_data <- sc_data[order(as.numeric(sc_data[, 3]), sc_data[, 1]), ]

    data_list  <- split(sc_data, f = sc_data$State)
    
    if (num == "best"){
        output <- sapply(data_list, function(x) x[1,1])
        output <- cbind(output, names(output))
    } else if(num == "worst") {
        output <- sapply(data_list, function(x) x[nrow(x),1])
        output <- cbind(output, names(output))
    } else {
        output <- sapply(data_list, function(x) x[num,1])
        output <- cbind(output, names(output))
    }    
    outputdf <- data.frame(output)
    colnames(outputdf) <- c("hospital", "state")
    outputdf
}