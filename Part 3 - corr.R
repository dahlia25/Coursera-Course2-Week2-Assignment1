# Part 3:
# Write a function that takes a directory of data files and a threshold for complete cases and calculates the correlation between
# sulfate and nitrate for monitor locations where the number of completely observed cases (on all variables) is greater than the 
# threshold. 

# The function should return a vector of correlations for the monitors that meet the threshold requirement. 

# If no monitors meet the threshold requirement, then the function should return a numeric vector of length 0.


# My Solution:

## Refer to Part 1 (pollutantmean.R) about this function:

multmerge <- function(path){
        filenames <- list.files(path="C:/.../, full.names = TRUE)
        datalist <- lapply(filenames, function(x){read.csv(file=x,header=TRUE)})
        Reduce(function(x,y) {rbind(x,y)}, datalist)
}

