## Put comments here that give an overall description of what your
## functions do

## Goal is to compute the inverse of a square invertible matrix and
## store the inverse in cache. This way if the inverse has to be computed
## more than once the inverse can be retrieved from the cache instead of
## always performing matrix inversion.

## Write a short comment describing this function

## creates a special matrix object with a set, get,
## setinverse, and getinverse method. Must be used
## in combination with "cacheSolve" function which
## returns the inverse of a square invertible matrix
## created as a special matrix object by "makeCacheMatrix".

makeCacheMatrix <- function(x = matrix()) {
        ## "x" has as its default value an empty matrix.
        
        x_inv <- NULL ## initialize the inverse of X to NULL
        set <- function(y) {   ## define the set method for the special matrix object
                x <<- y        ## This method allows you to reset the matrix without
                x_inv <<- NULL ## having to create a new object.
        }
        get <- function() x ## define the get method for the special matrix object
        setinverse <- function(inv) x_inv <<- inv ## define the setinverse method
        getinverse <- function() x_inv ## define the getinverse method
        list(set = set, get = get, setinverse = setinverse, ## collect methods in a list object
             getinverse = getinverse)
}


## Write a short comment describing this function

## Computes the inverse of a square invertible matrix
## stored as a special matrix object which is created 
## by the "makeCacheMatrix" function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        x_inv <- x$getinverse() ## get the inverse of matrix stored in cache
        ## this will be NULL if inverse hasn't been
        ## computed before
        if (!is.null(x_inv)) {  ## if cache contains the inverse return the inverse
                message("getting cached data")
                return(x_inv)
        }
        data <- x$get() ## get the square invertible matrix from special matrix object
        x_inv <- solve(data) ## compute the inverse
        x$setinverse(x_inv) ## store the inverse in the cache
        x_inv ## return the inverse
}
