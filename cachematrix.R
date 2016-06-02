## Functions for caching and solving the inverse of a matrix



## makeCacheMatrix will store both the original matrix and 
## the inverse of it (once it is sovled)

makeCacheMatrix <- function(x = matrix()) {
    invMat <- NULL
    set <- function(y) {
        x <<- y
        invMat <<- NULL
    }
    get <-function() x
    setInv <-function(solve) invMat <<- solve
    getInv <- function() invMat
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
    }


## Will solve and store the inverse of a matrix, or return it
## if it has already been solved

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invMat <- x$getInv()
    if(!is.null(invMat)) {
        message("getting cached data")
        return(invMat)
    }
    data <- x$get()
    invMat <- solve(data, ...)
    x$setInv(invMat)
    invMat
}
