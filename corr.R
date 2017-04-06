

correlations <- numeric()

corr <- function(directory, threshold = 0) {
   ##all files in the folder
        files_in_directory <- dir(directory)
        
        for (file_name in files_in_directory) {
               # print(file_name)
                data_with_na                   <- read.csv(paste(directory,file_name, sep = "/"))
                
                ## identify complete observations
                complete_list                  <- complete.cases(data_with_na[c("sulfate","nitrate")])
                number_of_complete             <- sum(complete_list)
                if(number_of_complete>threshold)
                {
                        nitrate<-data_with_na$nitrate[complete_list]
                        #print(sum(is.na(nitrate)))
                        sulfate<-data_with_na$sulfate[complete_list]
                        correlations<-c(correlations,cor(nitrate,sulfate))
                       # correlations<-c(correlations,sum(is.na(nitrate)),sum(is.na(sulfate)))
                      #  print(correlations)
                }

        } ## end of for loop
        correlations
}