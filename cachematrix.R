## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix" object that can cache its
## inverse. It returns a list of functions used to get/set the matrix
## itself and to get/set its cached inverse. The inverse is stored in
## the enclosing environment (using <<-) so it persists between calls.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" object
## returned by makeCacheMatrix. If the inverse has already been
## calculated and the underlying matrix hasn't changed since, this
## function retrieves the inverse from the cache instead of
## recomputing it, saving the cost of a fresh matrix inversion.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
