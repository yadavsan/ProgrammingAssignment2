## Caching the Inverse of a "matrix":

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(l = matrix()) {
        inv <- NULL
        set <- function(m) {
                l <<- m
                inv <<- NULL
        }
        get <- function() l
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" which is created by makeCacheMatrix function. If the inverse has already been 
## calculated (and the matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(l, ...) {
        ## Return a matrix that is the inverse of 'l' "matrix"
        inv <- l$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        l$setInverse(inv)
        inv
}
