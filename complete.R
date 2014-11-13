complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## "id" is an integer vector indicating the monitor ID numbers 
    ## to be used
    
    ## save original working directory
    originalwd <- getwd()
    
    ## set working directory
    setwd(directory)
    
    ## read data file(s)
    for(i in id) {
        filename <- paste(sprintf("%03d",i),".csv",sep="")
        data <- read.csv(file=filename,header=TRUE,sep=",")

        ## combine our datasets vertically
        ## if this is the first dataset
        if (i == id[1]) {
            myfulldata <- c(i,nrow(data[complete.cases(data),]))
            ## if this is our second dataset onward
        } else {
            myfulldata <- rbind(myfulldata,c(i,nrow(data[complete.cases(data),])))
        }
    }
    
    ## return to original working directory
    setwd(originalwd)
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    if(class(myfulldata) == "matrix") {
        myfulldataDF <- data.frame(myfulldata[,1],myfulldata[,2])
        colnames(myfulldataDF) <- c("id","nobs")
    } else {
        names(myfulldata) <- c("id","nobs")
        myfulldataDF <- data.frame(id=myfulldata["id"], nobs=myfulldata["nobs"])
    }
    
    myfulldataDF
    
}



