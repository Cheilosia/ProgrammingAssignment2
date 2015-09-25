## This set of functions sets up a cache that holds the inverse of a matrix and then checks whether that inverse
## already exists, and calculates it if it does not exist. 
## usage:
## newmatrix <- matrix(1:4, 2, 2)
## z <- makeCacheMatrix(newmatrix)              # This does not calculate the inverse yet
## cacheSolve(z)                                # Inverse calculated
## cacheSolve(z)                                # From second call on, inverse is taken from cache


## makeCacheMatrix sets up the cache that holds the inverse of the matrix. It also has functions for putting the inverse into
## the cache, and for retrieving it. makeCacheMatrix is actually a list: one can access the sub-function with the $ sign, as in
## makeCacheMatrix$get


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)    
}


## cacheSolve first checks whether the inverse exists in the cache, and if not, calculates it. solve is the R function that 
## calculates the inverse of a matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}