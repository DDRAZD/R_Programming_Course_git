


rankall <- function(outcome, num = "best") {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        ## set names for columns
        col_names         <-c("heart attack","heart failure","pneumonia")
        outcome_col       <-c(11,17,23)
        names(outcome_col)<-col_names
        
        ## Check that outcome is valid
        if(is.na(match(outcome, col_names)))
        {
                stop("invalid outcome")
        }
        
       
        
        outcome_df<-read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors=FALSE)
       
        relevant_df<-outcome_df[,c(2,7,outcome_col[outcome])]
        relevant_df_no_NA<-relevant_df[complete.cases(relevant_df),]
        names(relevant_df_no_NA)<-c("hospital","state","sortby")
        ##now sort all 
        df_arranged <-  arrange(relevant_df_no_NA,state,sortby,hospital)
        df_arranged_split_by_state<-split(df_arranged, df_arranged$state)
        get_hospital <- function(df)
        {
                if(num=="best") rank<-1
                else if(num=="worst") rank<-nrow(df)
                else rank <- num 
                
                df[rank,"hospital"]
        }
        hospital_names<-lapply(df_arranged_split_by_state,get_hospital)
        list_of_names <- names(hospital_names)
        data.frame(hospital=unlist(hospital_names),state=list_of_names,row.names=list_of_names)
                   
        
        
}
