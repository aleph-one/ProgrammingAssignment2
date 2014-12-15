# this function creates a wrapper around a matrix.
# the wrapper provides accessor functions for the matrix itself as well as its inverse
makeCacheMatrix <- function(x = matrix()) {
        xInverted <- NULL
        get <- function() x
        set <- function(y) {
                x <<- y
                xInverted <<- NULL
        }
        getInverse <- function() xInverted
        setInverse <- function(x) xInverted <<- x
        list(get = get,
             set = set,
             getInverse = getInverse,
             setInverse = setInverse)
}



# this function computes the inverse of the matrix wrapped by 'x' if not already cached.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xInverted <- x$getInverse()
        if(!is.null(xInverted)) {
                message("getting cached data")
                return(xInverted)
        }
        data <- x$get()
        xInverted <- solve(data, ...)
        x$setInverse(xInverted)
        xInverted
}
