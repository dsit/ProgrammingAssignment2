## The following functions can cache the inverse of a matrix
## rather than computing it repeatedly (after initial calculation).
## (Credit: Roger Peng's example code for caching the mean of a vector)

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then cacheSolve should retrieve the 
##inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        
        ## Check to see if the inversed matrix already exists.
        ## If so, return the existing object.
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## If inversed matrix doesn't exist, get the matrix supplied
        ## by the user.
        data <- x$get()
        
        ## Find the inverse of the matrix supplied by the user.
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
