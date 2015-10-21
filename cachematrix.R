## Implementation of cached matrix inverse

## Function to define matrix object with extra methods to enable the use of cached values

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(minv) m <<- minv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## function to warp around solve() which uses methods defined in makeCacheMatrix()
## that cached the value of inverse

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

## USAGE:
##
## source("cachematrix.R")
## X <- makeCacheMatrix(matrix(c(1, 3, 5, 4, 3, 7, 8, 10, 5), nrow = 3, ncol = 3))
## cacheSolve(X)