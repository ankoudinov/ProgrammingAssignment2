##  matrix inversion is time consuming
## this pair of functions caches the inverse matrix
## so it can be used multiple times

## makeCacheMatrix creates a list of functions to
## set the matrix
## get the matrix from cache
## set the inverse matrix
## get the inverse matrix from cache

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


## cacheSolve returns the cached inverse matrix 
## if it was already calculated
## else it calculates the inverse, saves it to the cache
## and returns as results of the cacheSolve

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
            message("getting cached matrix")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
