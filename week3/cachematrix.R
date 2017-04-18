## makeCacheMatrix takes a matrix as its input and creates
## a wrapper around the matrix that caches its inverse
makeCacheMatrix <- function(x = matrix()) {
        
        ## The cached inverse is initially null
        inv <- NULL
        
        ## set overwrites the cache matrix and reset the cached inverse
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## get returns the cache matrix
        get <- function() x
        
        ## setinverse caches the inverse of the matrix
        setinverse <- function(inverse) inv <<- inverse
        
        ## getinverse returns the cached inverse of the matrix
        getinverse <- function() inv
        
        ## the wrapper is returned
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}

## cacheSolve takes a cache matrix as its input and returns
## its inverse. If the inverse is cached the inverse is not
## recalculated
cacheSolve <- function(x, ...) {
        
        # read the inverse
        inv <- x$getinverse()
        
        # if it's not null the cached inverse is returned
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # otherwise the inverse is calculated ...
        data <- x$get()
        inv <- solve(data, ...)
        
        # ... cached ...
        x$setinverse(inv)
        
        # ... and returned
        inv
}
