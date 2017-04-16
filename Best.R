##assumes already set the working directory correctly

best <- function(state, outcome) {
        ## Read outcome data
        outcome_df<-read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors=FALSE)
        
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
        
        ## take only relevant state
        relevant_state<-subset.data.frame(outcome_df,outcome_df$State==state)
       
       
        if(outcome =="heart attack") { aranged_ouctome<-arrange(relevant_state, 
                                                                Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                                                                Hospital.Name)}
        if(outcome =="heart failure") { aranged_ouctome<-arrange(relevant_state,
                                                                Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                                                                Hospital.Name)} 
        if(outcome =="pneumonia")    { 
               
                aranged_ouctome<-arrange(relevant_state,Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,Hospital.Name)}
        
        as.character(aranged_ouctome[1,2])
        
}
