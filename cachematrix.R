## makeCacheMatrix and cacheSolve allows for caching the inverse of a matrix,
## so that costly computations are not necessary.

## makeCacheMatrix creates a list of functions to set & get values of matrix & inverse
makeCacheMatrix <- function(x = matrix()) {
    ## initialize i as NULL
    i <- NULL
    
    ## Set Matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## Get Matrix
    get <- function() x
    
    ## Set inverse of matrix
    setInverse <- function(inverse) i <<- inverse
    
    ## Get inverse of matrix
    getInverse <- function() i
    
    ## List of functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## cacheSolve finds inverse of matrix + checks if it already has a value
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    
    ## Checks if inverse is already set & returns it
    if(!is.null(i)) {
        message("getting cached data")
        return (i)
    }
    
    ## Get the matrix
    data <- x$get()
    
    ## Computes inverse of matrix
    i <- solve(data, ...) 
    
    ## Sets the inverse & returns value
    x$setInverse(i)
    i
}
