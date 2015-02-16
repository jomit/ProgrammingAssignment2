## Functions to cache matrix calculation output for performance and optimization
##==========================================================================================
## "makeCacheMatrix" function creates a special matrix with helper functions
## "cacheSolve" function takes the special matrix created by makeCacheMatrix function as argument
## and stores the inverse matrix in cachefor optimization

## Creates a special matrix containing the list of functions to 
## set the value
## get the value
## set the inverse
## get the inverse


makeCacheMatrix <- function(currentMatrix = matrix()) {
    inverse <- NULL
    set <- function(newMatrix) {
        currentMatrix <<- newMatrix
        inverse <<- NULL
    }
    get <- function() currentMatrix
    setinverse <- function(newinverse) inverse <<- newinverse
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Caches the inverse of the special matrix created using makeCacheMatrix function 

cacheSolve <- function(specialMatrix) {
    
    ## Return a matrix that is the inverse of 'specialMatrix'
    
    inverseValue <- specialMatrix$getinverse()
    if(!is.null(inverseValue)) {
        message("getting cached inverse data")
        return(inverseValue)
    }
    inverseValue <- solve(specialMatrix$get())
    specialMatrix$setinverse(inverseValue)
    inverseValue
}


# Usage
# > mymatrix <- makeCacheMatrix(rbind(c(1, -1/4), c(-1/4, 1)))
# > cacheSolve(mymatrix)
