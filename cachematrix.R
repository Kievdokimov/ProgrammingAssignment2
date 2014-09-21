## These two funcionions are designed to cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- matrix()
        set <- function(y) {
                x <<- y
                inv <<- matrix()
        }
        get <- function() x
        setinv <- function(i) inv <<- i
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve  retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
        
        
        inv <- x$getinv()
        data <- x$get()
        if(!is.na(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
