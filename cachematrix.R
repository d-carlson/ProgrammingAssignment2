## Functions to cache the inverse of a matrix
## Two functions: makeCacheMatrix and cacheSolve

## Create a special "matrix" that is a list containing
## four functions to set and get a matrix and
## set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
    set <- function(y) {
            x <<- y
            m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Compute the inverse of the special "matrix" created
## with the function above, but first check to see if 
## the inverse is already available. If so get the
## cached inverse and skip the computation. Otherwise
## compute the inverse of the matrix and set result in
## the cache

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
