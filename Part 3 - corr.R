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

combined_data <- multmerge("C:/.../")

airdata <- na.omit(combined_data)


## Create corr function:

corr <- function(directory, threshold = 0) {
        directory <- airdata
        
        ## Initiate a new vector to store future values from FOR loop function:
        correlation <- vector(mode="numeric", length = 0)
        
        ## Remember to specify 'id' for the FOR loop.
        id = 1:332
        for(i in id) {
                id_set <- subset(airdata, airdata$ID == i)
                good <- complete.cases(id_set) 
                complete <- id_set[good, ]      ## subset the complete cases from selected 'id'
                
                if(nrow(complete) > threshold) {
                        cor_vector <- cor(complete[["sulfate"]], complete[["nitrate"]])
                                ## use [[ to extract specified elements of a list or data frame.
                        
                        correlation <- append(correlation, cor_vector)          ## use append() to add elements into vectors
                }
        }
        return(correlation)
}

## Testing to see if function works:

> cr <- corr(airdata, 150)
> head(cr)
[1] -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667 -0.07588814

> summary(cr)
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
-0.21057 -0.04999  0.09463  0.12525  0.26844  0.76313 

> cr <- corr(airdata, 400)
> head(cr)
[1] -0.01895754 -0.04389737 -0.06815956 -0.07588814  0.76312884 -0.15782860

> summary(cr)
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
-0.17623 -0.03109  0.10021  0.13969  0.26849  0.76313 

> cr <- corr(airdata, 5000)
> summary(cr)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##                                                

> length(cr)
[1] 0

> cr <- corr(airdata)
> summary(cr)
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
-1.00000 -0.05282  0.10718  0.13684  0.27831  1.00000 

> length(cr)
[1] 323
