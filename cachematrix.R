## The following two functions will create an invertible, square matrix for caching and
## will return its inverse.

## The following function creates the invertible, square matrix to be cached.

makeCacheMatrix <- function(x = matrix()) {
    mtrxinv <- NULL  ## sets placeholder for the inverse as an empty set
    set <- function(y) {
        x <<- y
        mtrxinv <<- NULL
        }  ## sets a matrix, using the originally-provided matrix and again sets inverse to blank
  
    get <- function() x  ## returns the original matrix x
    setinverse <- function(inverse_m) mtrxinv <<- inverse_m  ## sets the matrix inverse to be
    ## equal to mtrxinv
    getinverse <- function() mtrxinv  ## retrieves/returns the inverse of the matrix
    
    list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function returns the inverse of a matrix.
## If the selected matrix has previously been cached, we fetched the previous cached inverse.

cacheSolve <- function(x, ...) {
    mtrxinv <- x$getinverse()  ## try to find the cached inverse of the provided x matrix
    if(!is.null(mtrxinv)) {
        message("getting cached data")  ## if you look in x$getinverse and come up with something,
                                        ## just fetch the cached inverse.
        return(mtrxinv)
        }
    data <- x$get()  ## Back in the makeCacheMatrix function, set x to be the provided matrix
    mtrxinv <- solve(data, ...)  ## Here, explictly use the pre-set solve function to derive the inverse.
    x$setinverse(mtrxinv)  ## cache the inverse
    mtrxinv
        
    ## Return a matrix that is the inverse of 'x'
}
