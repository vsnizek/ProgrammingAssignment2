## This set of function calculate an inverse matrix of a square invertible matrix 
## and caches it for eventual further use

## makeCacheMatrix is special function containing 
## a list of auxilary functions to
##     1 set the value of the matrix
##     2 get the value of the matrix
##     3 set the value of the inverse matrix
##     4 get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL  # inverse matrix definition
    set <- function(y) {
        x <<- y
        s <<- NULL
    } # set value y to matrix
    get <- function() x # get value to matrix
    setSolve <- function(solve) s <<- solve # set value of the inverse matrix
    getSolve <- function() s # get value of the inverse matrix
    list(set = set, get =  get, setSolve = setSolve, getSolve= getSolve) # final list of functions
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix hasn't changed), then the cacheSolve
## retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <- x$getSolve()
    if(!is.null(s)) {
        message("getting cached data"
        return(s)
        } ## if x is found in cache, then return the inverse matrix s without any computation
    data <- x$get()
    s <- solve(data, ...) ## calculates the inverse matrix of x which wasn't found in cache
    x$setSolve(s) ## save x and it's inverse matrix to cache
    s ## return the inverse matrix
}
