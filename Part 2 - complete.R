# Part 2:
# Write a function that reads a directory full of files and reports the number of completely observed cases in each data file. 
 
# The function should return a data frame where the first column is the name of the file and the second column is the number of 
# complete cases.

# My Solution:

## Refer to Part 1 (pollutantmean.R) for this function

multmerge <- function(path){
        filenames <- list.files(path="C:/.../", full.names = TRUE)
        datalist <- lapply(filenames, function(x){read.csv(file=x,header=TRUE)})
        Reduce(function(x,y) {rbind(x,y)}, datalist)
}

combined_data <- multmerge("C:/.../")

airdata <- na.omit(combined_data)

## Since we are only interested in the 'id' and 'nobs' (# of observations), I removed all irrelevant columns.
## This also means that only "complete observed cases", or cases with no missing or NA values, remain.
airdata$Date <- NULL
airdata$sulfate <- NULL
airdata$nitrate <- NULL


## Create function complete:
complete <- function(directory, id) {
        directory <- airdata
        
        ## Initiate an empty data frame to store computated values that run through the FOR loop function: 
        combined_df <- data.frame()
        
        for(i in id) {
                id_num <- subset(airdata, airdata$ID == i)
                df <- data.frame(i, nrow(id_num), stringsAsFactors = FALSE)
                combined_df <- rbind(combined_df, df)
        }
        
        ## Use the colnames() function to name the columns in 'combined_df':
        colnames(combined_df) <- c("id", "nobs")
        return(combined_df)
}


## Testing to see if our function works:
> complete(airdata, 1)
  id nobs
  1  117

> complete(airdata, c(2, 4, 8, 10, 12))
  id nobs
1  2 1041
2  4  474
3  8  192
4 10  148
5 12   96

> complete(airdata, 30:25)
  id nobs
1 30  932
2 29  711
3 28  475
4 27  338
5 26  586
6 25  463

> complete(airdata, 3)
  id nobs
1  3  243
