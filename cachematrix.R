# Assignment: Caching the Inverse of a Matrix
# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than computing it repeatedly (there
# are also alternatives to matrix inversion that will not be discussed here).
# This assignment is to write a pair of functions that cache the inverse of a
# matrix. For simplicity the input matrix is assumed to be invertible.

# Example Usage:
# > X <- matrix(rnorm(9), nrow = 3, ncol=3)  |  create a 3x3 matrix X
# > cacheX <- makeCacheMatrix(X)             |  create the special cache matrix
# > cacheX$get()                             |  return the cache matrix
# > cacheSolve(cacheX)                       |  solve for the inverse of X 
# > cacheSolve(cacheX)                       |  solve for the inverse of X again
#                                            |   by returning the cached inverse

# 'makeCacheMatrix':
# This function creates a special "matrix" object that can cache its inverse. 
# This takes the form of creating and returning a list of four functions.
# 1: 'set' is a function that sets the value of the matrix
# 2: 'get' is a function that gets the cached value of the matrix if available
# 3: 'setinv' is a function that sets the value of the inverse matrix
# 4: 'getinv' is a function that gets the cached value of the inverse matrix

    makeCacheMatrix <- function(x = matrix()) {
    
            # xinv is the result of the inversion, initialised to NULL
            xinv <- NULL  
            
            # the 'set' function to cache the input matrix 'x'
            set <- function(y) {
                    x <<- y             # use '<<-' to assign a value to an 
                    xinv <<- NULL       # an object in an environment different
                                        # from the current environment
            }
            
            # the 'get' function returns the input matrix 'x'
            get <- function() x
            # the 'setinv' function to cache the inverse matrix 'xinv'
            setinv <- function(inverse) xinv <<- inverse
            # the 'getinv' function returns the inverse matrix 'xinv'
            getinv <- function() xinv
            # return a list containing these 4 functions
            list(set = set, get = get, setinv = setinv, getinv = getinv)
    }

# 'cacheSolve':
# This function computes the inverse of the special "matrix" returned by
# 'makeCacheMatrix' above. If the inverse has already been calculated, then
# 'cacheSolve' should retrieve the inverse from the cache.

    cacheSolve <- function(x, ...) {
    
            # get the cached version of the inverse matrix 'xinv'
            xinv <- x$getinv()
            
            # if 'xinv' is not NULL then return the cached version of 'xinv'
            if(!is.null(xinv)) {
                    message("getting cached data")
                    return(xinv)
            }
            
            # otherwise use 'get' to fetch the cached version of 'x'
            data <- x$get()
            # use 'solve' to generate the inverse matrix 'xinv'
            xinv <- solve(data, ...)
            # use 'setinv' to cache the inverse matrix 'xinv'
            x$setinv(xinv)
            # return the 'inverse matrix 'xinv' 
            xinv
    }