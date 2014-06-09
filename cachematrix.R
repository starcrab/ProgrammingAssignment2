## A Pair of functions to create a special matrix, calculate inverse & cache the result
## to save computer time

## Create a special Matrix and list of 4 functions to set, get the matrix as well as the inverse results

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setmatrix <- function(y, ...){
        x <<- matrix(y, ...)
        inv <<- NULL
    }
    get <- function() x
    setsolve <- function(inv2) inv <<- inv2
    getsolve <- function() inv
    list(setmatrix = setmatrix, get = get, setsolve = setsolve, getsolve = getsolve)

}


## Calculates the inverse of the special Matrix and cache it, check if it's been calculated
## if calculated, get the result from cache and skip computation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getsolve()
    if(!is.null(inv)){
        message("Getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setsolve(inv)
    inv
}
