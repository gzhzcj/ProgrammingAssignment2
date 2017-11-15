## Provide functions to compute the inverse of a square matrix and cache the result.
## For example, if X is a square invertible matrix, 
## Step 1, use c<-makeCacheMatrix(x) to get an object
## Step 2, cacheSolve(c) to get the result.
## In step 2, if there's cached data, skip the computing and return the result immediately.
## If there's no cached data, compute the result first, cache and return the result.

## makeCacheMatrix function provides the prototype of the object

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inversed) m <<- inversed
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)        
}


## cacheSolve function
## Arguments
## x    an object returned by makeCacheMatrix function
##      which stores a square invertible matrix
## Value 
##      the inverted matrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        message("computing data...")
        m <- solve(data, ...)
        message("caching data...")
        x$setinverse(m)
        m
}
