rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    state_data <- data[data$State == state, ]
    if (nrow(state_data) < 1 ) {
        stop("invalid state")
    }
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
    
    sc_data <- state_data[, c(2, outcomeID)]
    sc_data <- sc_data[sc_data[, 2] != "Not Available", ]
    sc_data <- sc_data[order(as.numeric(sc_data[, 2]), sc_data[, 1]), ]
    
    if (num == "best"){
        output = sc_data[1,1]
    } else if(num == "worst") {
        output = sc_data[nrow(sc_data), 1]
    } else {
        output = sc_data[num, 1]
    }
    output
}