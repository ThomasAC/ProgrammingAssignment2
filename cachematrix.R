## The "makeCacheMatrix" and "cacheSolve" functions can calculate the inverse of a matrix and cache its value
## to avoid computing it repeatedly.

## Creates a makeCacheMatrix, as a list of 4 functions to set, get and cache a matrix and its inverse.  

makeCacheMatrix <- function(x = matrix()) {
        ## Sets inverse matrix to NULL
        Inv <- NULL
        ## the set function allows to cache matrix and inverse value
        set <- function(y) { 
            x <<- y 
            Inv <<- NULL 
        }
        ## The get function returns the matrix value
        get <- function() x
        ## the setInverse function sets the inverse matrix
        setInverse <- function(Inverse) {
        Inv <<- Inverse
        }
        # the getInverse function returns the inverse matrix, NULL if not yet calculated.
        getInverse <- function() Inv
        ## returns themakeCacheMatrix object as a list of 4 functions
        list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## cacheSolve function returns the inverse of a matrix,  using the cached value 
## or recalculating it if necessary, caching the newly calculated value

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
        Inv <- x$getInverse()
        ## Returning inverse value from the cache if the computation is not necessary
        if(!is.null(Inv)) {
        message("getting cached data")
        return(Inv)
        }
        ## Feed the new matrix for calculation
        data <- x$get()    
        ## Calacute inverse matrix
        Inv <- solve(data, ...)
        ## Cache value of Inverse matrix and returns it
        x$setInverse(Inv)
        Inv
}
