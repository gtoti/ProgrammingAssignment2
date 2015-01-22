## PA 2 - caching the inverse of a matrix

## The function makeCacheMatrix receives in input a matrix and creates 
## a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)        
}


## The function cacheSolve computes the inverse of the matrix object
## created by makeCacheMatrix

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
