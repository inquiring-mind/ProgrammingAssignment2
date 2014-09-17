## Functions that cache a Matrix

##  This function creates a special "matrix", 
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {    
    i <- NULL
    set <- function( matrix ) {
        m <<- matrix
        i <<- NULL
    }
    get <- function() m 
    setInverse <- function(inverse)  i <<- inverse
    getInverse <- function() i
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The following function calculates the inverse of the special "matrix" 
## created with the above function. However, it first checks to see if 
## the inverse has already been calculated. If so, it gets the inverse from the 
## cache and skips the computation. Otherwise, it calculates the inverse of the 
## data and sets the value of the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if( !is.null(m) ) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data) %*% data
    x$setInverse(m)
    m
}
