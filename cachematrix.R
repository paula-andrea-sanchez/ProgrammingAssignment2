## This function can first looked up in the cache rather than recomputed the inverse matrix by using the function solve in R. 

## Creates a special "matrix", which is a list containing a function to: 1. set the value of the matrix, 2. get the value of the matrix, 3.set the value of the mean and 4.  get the value of the mean.


makeCacheMatrix <- function(x = matrix()) {
    invs <- NULL
    set <- function(y) {
        x <<- y
        invs <<- NULL
    }
    get <- function() x
    setinvs <- function(solve) invs <<- solve
    getinvs <- function() invs
    list(set = set, get = get,
         setinvs = setinvs,
         getinvs = getinvs)
  }


## Calculates the inverse matrix of a  special "matrix" created with the above function. However, it first checks to see if the inverse matrix has already been calculated. If so, it `get`s the inverse matrix from the cache and skips the computation.


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invs <- x$getinvs()
    if(!is.null(invs)) {
          message("getting cached data")
          return(invs)
    }
    data <- x$get()
    invs <- solve(data, ...)
    x$setinvs(invs)
    invs
}
