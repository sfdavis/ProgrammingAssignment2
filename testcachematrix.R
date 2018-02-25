source("cachematrix.R")

## This is a test script for verifing Program Assignment #2
## Usage:
##      > source("testcachematrix.R")
##      > testCacheMatrix()

testCacheMatrix <- function() {
    tryCatch({
        vect <- c(1:9)
        makeCacheMatrix(vect)
    }, error = function(e) {
        message("TEST1 - Expect error for feeding non-matrix to makeCacheMatrix | actual error message = ", e)
    })
    
    tryCatch({
        nonSquareMatrix <- matrix(runif(6), nrow=2, ncol=3)
        makeCacheMatrix(nonSquareMatrix)
    }, error = function(e) {
        message("TEST2 - Expected Error for feeding non-square matrix to makeCacheMatrix | actual error message = ", e)
    })
    
    tryCatch({
        ## Create two square matricies
        X <- matrix(runif(9), nrow=3, ncol=3)
        Y <- matrix(runif(9), nrow=3, ncol=3)
        
        ## Create special lists for holding cached matricies and their inverses
        x = makeCacheMatrix(X)
        y = makeCacheMatrix(X)
        
        message("TEST3 - see if we have cache of X - it shouldn't be cached yet and should give us a message saying it is calculating it\n")
        Xinverse = cacheSolve(x)
        
        message("\nSee if X multiplied by Xinverse gives the identity matrix as it should\n")
        
        print(X %*% Xinverse)
        
        message("\nTEST4 - see if we have cache of X - it should be cached now and give us a message saying so\n")
        Xinverse = cacheSolve(x)
        
        message("\nSee if X multiplied by cached Xinverse STILL gives the identity matrix as it should\n")
        
        print(X %*% Xinverse)
        
        message("\nTEST5 - make sure y is not using xinverse but its own inverse - it should say Calculating...\n")
        Yinverse = cacheSolve(y)
        
        
    }, error = function(e) {
        message("TEST3 - Unexpected error message = ", e)
    })
}