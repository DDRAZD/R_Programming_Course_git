## complete observation means both sulfur and nitrogen are non NA
##prep for file reads: ## set this path when starting if different
path <- "C:/Users/s4653591/Documents/DATA_SCIENCE/R_programing_course"
setwd(path)

complete <-function(directory, id=1:332){
        number_of_monitors_to_read <-length(id)
        
        read_data_frames <- data.frame(id = 0, nobs=0)
        
        for (i in 1:number_of_monitors_to_read) {
                
                if(id[i]<10){file_number<-paste("00",id[i],sep = "")}
                else if (id[i]>=100) {file_number<-id[i]}
                else {file_number<-paste("0",id[i],sep = "")}
                
                file_name                      <- paste(directory,"/",file_number,".csv", sep="")
                data_with_na                   <- read.csv(file_name)
                
                ## identify complete observations
                complete_list                  <- complete.cases(data_with_na[c("sulfate","nitrate")])
                number_of_complete             <- sum(complete_list)
                ##update data.frame
                
                if(i==1){
                        read_data_frames[1,1]  <-id[1]
                        read_data_frames[1,2]  <- number_of_complete
                }
                else {
                        add_to_data_frame <- data.frame(id = id[i],nobs = number_of_complete)
                        read_data_frames <- rbind(read_data_frames,add_to_data_frame)
                }
        }##end of for loop
        read_data_frames ## return the final data frame
        
}