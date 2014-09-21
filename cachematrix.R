## R Programming class, Week 2 programming assignment

## Creates a special Matrix object capable of caching
## its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    # Set the matrix value, deletes old cached inverse
    set <- function(m) {
        x <<- m
        inv <<- NULL
    }
    
    # Get matrix value
    get <- function() x
    
    # Set a value for the cached inverse matrix
    setsolve <- function(inverse) inv <<- inverse
    
    # Get the cached matrix value
    getsolve <- function() inv
    
    list( 
        get = get, set = set,
        getsolve = getsolve,
        setsolve = setsolve)
}


## Returns the inverse of a matrix, caching the result

cacheSolve <- function(x, ...) {
    # If the passed matrix object doesn't have a cached
    # inverse value, calculate one, then store it.
    cache <- x$getsolve()
    if (! is.null(cache)) {
        # For debugging purposes, uncomment the following line
        # to verify that the cached value is being returned.
        # message("Returning cached inverse")
        return(cache)
    }
    data <- x$get()
    cache <- solve(data, ...)
    x$setsolve(cache)
    cache
}
