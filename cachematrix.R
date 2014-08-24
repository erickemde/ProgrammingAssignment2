## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function caches a matrix and its inverse, 
##     allowing you to set and get either

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
## Checks to see if the matrix's inverse has already been cached. 
## If it has, it returns the cached inverse matrix.
## If it hasn't, it uses solve() to create the inverse matrix and caches it.

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
