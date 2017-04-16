##assumes already set the working directory correctly

best <- function(state, outcome) {
        ## Read outcome data
        outome<-read.csv("outcome-of-care-measures.csv")
        
        ## Check that state and outcome are valid
        ## The hospital name is the name provided in the Hospital.Name variable. 
        ## The outcomes can be one of "heart attack", "heart failure",
        ## or "pneumonia". 
        
        if(is.na(match(outcome, c("pneumonia","heart attack","heart failure"))))
              {
                stop("invalid outcome")
        }
        if(is.na(match(state,state.abb)))
           stop("invalid state")
        
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
        ##handling ties
        
}