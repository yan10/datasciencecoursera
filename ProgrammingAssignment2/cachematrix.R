## Put comments here that give an overall description of what your
## functions do

## The first function creates matrix andit's inverse and store in cache.
## The second function compute the inverse of the matrix from x.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        ## set function stores the matrix in cache, get function recalls the matrix from cache
        set <- function(y) {
                x <<- y
                inverse <- NULL
        }
        get <- function() x
        ## setinverse function stores the inversed matrix in cache, getinverse function recalls the inversed matrix from cache
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        ## Check if there exists the inversed matrix already, if so, return it directly from cache
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        ## calculate the inverse of matrix, got from cache.
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
