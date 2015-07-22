## Utility: cache the inverse of a matrix.
## Why do this: matrix inversion is usually a costly computation, and we don't
## need to recompute the inverse if the ## matrix has not changed in a loop.

## This function creats a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        # inv represents for the inverse matrix
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # make a function to set the value of the matrix to be solved and 
        # the initial value of the inverse
        
        get <- function() {x}
        # make a function to get the value of matrix
        
        setinverse <- function(inverse) {inv <<- inverse}
        # make a function to pass the inverse to inv
        
        getinverse <- function() {inv}
        # make a function to return the value of the inverse
        
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        # list the details of all functions
}


## This function calculates the inverse of the special "matrix" created with
## the above function. However, it first checks to see if the inverse has
## already been calculated. If so, it gets the inverse from the cache and skips
## the computation. Otherwise, it calculates the inverse of the matrix and sets
## the inverse matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        # use 'getinverse' to get the inverse
        
        if(!is.null(inv)) {
                message("getting cached inverse matrix")
                return(inv)
        }
        # check inv if the inverse has already been there,
        # if so, return the value and skip the computation
        
        matrix <- x$get()
        # use 'get' to get the matrix
        
        inv <- solve(matrix, ...)
        # calculate the inverse
        
        x$setinverse(inv)
        # use 'setinverse' to set the value of the inverse to inv
        
        inv
        # return the inverse
}
