##week 2 hw part 1

##prep for file reads: ## set this path when starting if different
path <- "C:/Users/s4653591/Documents/DATA_SCIENCE/R_programing_course"
setwd(path)

pollutantmean <- function (directory, pollutant, id=1:332) {
        ## directory is a character vector with length 1
        ## pollutant is a chracter vector with length 1
        ## id is the integer vector indicating which monitor results to use
        
        ## function returns the mean of all pollutant results (ignoring NA)
        ## mean will be on all monitors
        
        observations<-numeric()
        
        number_of_monitors_to_read <-length(id)
      #  print(number_of_monitors_to_read)
        for (i in 1:number_of_monitors_to_read) {
                
                if(id[i]<10){file_number<-paste("00",id[i],sep = "")}
                else if (id[i]>=100) {file_number<-i}
                else {file_number<-paste("0",id[i],sep = "")}
                
                file_name                      <- paste(directory,"/",file_number,".csv", sep="")
                data                           <-read.csv(file_name)
          
                observations_with_na           <- data[,pollutant]
                observations_this_file         <- observations_with_na[!is.na(observations_with_na)]
                observations                   <-c(observations,observations_this_file)
               
              
        }## end of for loop
       
      mean(observations)
}

