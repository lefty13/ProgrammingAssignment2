## These functions provide a new object that cache the inverse of a 
## matrix so it does not need to be computed each time.

## makeCacheMatrix creates a special matrix "object" that caches its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    # cached inverse
    inv <- NULL
    
    #define the set function to set x and clear the inverse
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    #define the get function - just return x
    get <- function() x
    
    #set the inverse
    setInverse <- function(inverse) inv <<- inverse
    
    #get the inverse
    getInverse <- function() inv
    
    # return a list with all four functions
    list( set = set, get = get, setInverse = setInverse, 
          getInverse = getInverse)
}




## cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix().
## If the inverse has already been computed, it is retrieved from the cache.
cacheSolve <- function(x, ...) {
    # retrieve the cached inverse, if any
    inv <- x$getInverse()
    
    # check if inverse is already cached
    if (!is.null(inv)) {
        message( "getting cached inverse")
        return(inv)
    }
    
    # not cached; must compute it.  Start by getting the matrix values
    data <- x$get()
    
    # calculate the inverse
    inv <- solve(data)
    
    # Store the inverse in the cache
    x$setInverse(inv)
    
    # return the inverse
    inv
        
}
