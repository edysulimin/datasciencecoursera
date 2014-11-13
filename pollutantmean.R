pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the 
    ## mean; either "sulfate" or "nitrate"
    
    ## "id" is an integer vector indicating the monitor ID numbers 
    ## to be used
    
    ## save original working directory
    originalwd <- getwd()
    
    ## set working directory
    setwd(directory)
    
    ## read data file(s)
    for(i in id) {
        data <- read.csv(file=paste(sprintf("%03d",i),".csv",sep=""),header=TRUE,sep=",")
        
        ## combine our datasets vertically
        ## if this is the first dataset
        if (i == id[1]) {
            myfulldata <- data
        ## if this is our second dataset onward
        } else {
            myfulldata <- rbind(myfulldata,data)
        }
    }
    
    ## return to original working directory
    setwd(originalwd)
    
    ## Return the mean of the pollutant accross all monitors list
    ## in the 'id' vector (ignoring NA values)
    ## and round to 3 decimal places
    round(mean(myfulldata[,eval(pollutant)],na.rm=TRUE), digits = 3)
}



