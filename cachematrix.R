## The two functions below, makeCacheMatrix and cacheSolve, take a matrix
## as input and then calculate and cache its inverse.

## First, makeCacheMatrix takes the matrix input and creates a 
## special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(dummy) {
                x <<- dummy
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Second, cacheSolve checks to see if the inverse of the matrix has 
## already been calculated (and cached), given that the matrix has not changed.
## If the matrix has not been changed, cacheSolve returns the cached inverse.
## If the matrix has changed, cacheSolve computes the inverse of the matrix
## returned by makeCacheMatrix, and returns it.

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}