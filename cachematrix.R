## This file contains two fuctions to hold and cache a matrix and its inverse

## makeCacheMatrix is used to create a list to cache a matrix and its inverse and contains  
## getters and setters functions to get/set both the original matrix and the inverse matrix

makeCacheMatrix <- function(X = matrix()) {
    ## Validate the input parameter is a square matrix - do not check for invertibility since that is
    ## assumed in the assignment instructions
    if ( class(X) != "matrix" ) { 
        stop("makeCacheMatrix - argument must be a matrix!")
    }
    if ( nrow(X) != ncol(X) ) {
        stop("makeCacheMatrix - argument must be a square matrix to be invertible!")
    }
    
    ## initialize the X matrix inverset to NULL
    Xinverse <- NULL
    
    ## Create the set function to set the stored value of the matrix and reset its inverse
    set <- function(Y) {
        X <<- Y
        Xinverse <<- NULL
    }
    
    ## Create the get function to return the set value of the matrix
    get <- function() X
    
    ## Create the functions to set and get the inverse of X
    setinverse <- function(solve) Xinverse <<- solve
    getinverse <- function() Xinverse
    
    ## return the list containing functions for our cached matrix
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve returns the cached version of the matrix stored in list x if it has been
## previously calculated.  Otherwise, it computes the inverse and stores it back into the
## list for future use

cacheSolve <- function(x, ...) {
    ## x is the list containing the set, get, setinverse, and getinverse functions along
    ## the stored X and Xinverse matricies - for real code x shold be validated but was
    ## not specified in the assignment so skipped
    
    ## See if the matrix that is the inverse of 'X' has previously been calculated and cached
    Xinverse <- x$getinverse()
    if( !is.null(Xinverse)) {
        message("Getting cached inverse of the matrix")
        return(Xinverse)
    }
    
    # inverse has not been computed yet - get the initial matrix
    X <- x$get()
    # compute the inverse of the with the solve function
    message("Calculating the inverse of the matrix and caching the value")
    Xinverse <- solve(X)
    x$setinverse(Xinverse)
    Xinverse    
}
