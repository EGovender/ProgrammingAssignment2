## These functions use chachine of variables to return the inverse of a matrix.
## If the inverse of a matrix has already been computed then the cached result is returned,
## otherwise the result is computed, stored in the cache and returned.

## This function gets and sets the value of a cached matrix. It also gets and sets the inverse
## value of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inverse <<- solve
        getinv <- function() inverse
        list(set = set, get = get, 
                setinv = setinv,
                getinv = getinv)
                
}


## This function gets the inverse of a matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinv()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinv(inverse)
        inverse
}
