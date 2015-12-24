## Functions makeCacheMatrix and cacheSolve perform inverse operations on matrices

## makeCacheMatrix saves a matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
        x <<- y
        i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)
}


## cacheSolve returns the inverse of a matrix. If the inverse has already been calculated, 
## cacheSolve will return the cached data and notify the user that cached data has been used.

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        i
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
