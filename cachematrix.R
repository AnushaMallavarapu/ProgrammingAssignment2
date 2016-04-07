
## A pair of functions that cache the inverse of a matrix
## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## Initialize the inverse property
        m <- NULL
        ## Method to set the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## Method to get the matrix
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        ##Return a list of methods
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        ## Just return the inverse if its already set
        if( !is.null(m) ) {
        message("getting cached data")
        return(m)
        }
        ## Get the matrix from our object
        data <- x$get()
        ## Calculate the inverse
        m <- solve(data, ...)
        ## Set the inverse 
        x$setInverse(m)
        ## Return the matrix
        m
}

