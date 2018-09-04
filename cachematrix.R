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

## Please find below output
> specialMatrix<- makeCacheMatrix(matrix(c(10,15,18,20),2, 2))
> specialMatrix$get() ## Check the "matrix" is correct of not
     [,1] [,2]
[1,]   10   18
[2,]   15   20
> specialMatrix$getInverse() 
NULL
> cacheSolve(specialMatrix) ## Inverse of Special matrix
           [,1]       [,2]
[1,] -0.2857143  0.2571429
[2,]  0.2142857 -0.1428571

