## This function caches the inverse of a matrix once it has been
## computed to avoid the need for recalculation. It is an example of
## the more general concept and power of cacheing.

## makeCacheMatrix creates a list structure that defines a set of 
## functions and the location of their values. The syntax is
## y <- makeCacheMatrix(A) where A is square matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



## cacheSolve calculates the inverse of a matrix [using "solve"]
## and caches it if the value is not already cached, and retrieves
## the cached value if it has already been calculated and cached.
## The syntax is x <- cacheSolve(y) where y is from makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m   
}
