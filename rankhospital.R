##The num argument can take values "best", "worst", or an integer indicating the ranking
##(smaller numbers are better). If the number given by num is larger than the number of hospitals in that
##state, then the function should return NA. Hospitals that do not have data on a particular outcome should
##be excluded from the set of hospitals when deciding the rankings

rankhospital <- function(state, outcome, num = "best") {
        
        ## set names for columns
        col_names         <-c("heart attack","heart failure","pneumonia")
        outcome_col       <-c(11,17,23)
        names(outcome_col)<-col_names
        
        ## Check that state and outcome are valid
        if(is.na(match(outcome, col_names)))
        {
                stop("invalid outcome")
        }
        if(is.na(match(state,state.abb)))
                stop("invalid state")
        
        ## Read outcome data
        outcome_df<-read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors=FALSE)
        relevant_state<-subset.data.frame(outcome_df,outcome_df$State==state)
        relevant_df<-relevant_state[,c(2,7,outcome_col[outcome])]
       
        
        ##get rid of NAs (only after this was reduced to only 3 columns)
        relevant_df_no_NA<-relevant_df[complete.cases(relevant_df),]
        names(relevant_df_no_NA)<-c("hospital","state","sortbty")
        aranged_ouctome<-arrange(relevant_df_no_NA,sortbty, hospital)
        
        
       
         if(num=="best") rank<-1
        else if(num=="worst") rank<-nrow(aranged_ouctome)
        else rank <- num
        
        aranged_ouctome[rank,1]
        
        
        
        

}
