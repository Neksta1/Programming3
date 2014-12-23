best <- function(state, outcome) {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with lowest 30-day death
    ## rate
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
    
    sc_data = state_data[, c(2, outcomeID)]
    best = sc_data[which(sc_data[, 2]== min(as.numeric(sc_data[, 2]),
                                            na.rm = TRUE)),]
    sort(best[, 1])[1]
}