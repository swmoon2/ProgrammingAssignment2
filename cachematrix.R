## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(m = matrix()) {
    inv_m <- NULL
    setm <- function(x) {
        m <<- x
        inv_m <<- NULL
    }
    getm <- function() m
    setinvm <- function(invm) inv_m <<- invm 
    getinvm <- function() inv_m
    
    list(setm = setm, getm = getm,
         setinvm = setinvm,
         getinvm = getinvm)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_m <- x$getinvm()
    if(!is.null(inv_m)) {
        message("getting cached data")
        return(inv_m)
    }
    m <- x$getm()
    inv_m <- solve(m, ...)
    x$setinvm(inv_m)
    inv_m
}
