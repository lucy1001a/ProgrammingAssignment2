## This pair of functions are able to cache the inverse of a matrix

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## It creates a list of 4 functions: 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
                ## initialize the inverser matrix to NULL
                inv <- NULL
                ## set/reset the value of the matrix inv
                set <- function(y) {
                        x <<- y
                        inv <<- NULL
                }
                ## get function for the origin matrix
                get <- function() x
                ## store the inverse matrix
                setinv <- function(inverse) inv <<- inverse
                ## get the inverse matrix stored
                getinv <- function() inv
                list(set = set, get = get, 
                     setinv = setinv, 
                     getinv = getinv)
}


## cacheSolve takes a caching matrix created with makeCacheMatrix
## It either get the cache inverse matrix from makeCacheMatrix
## or calculate the inverse matrix and then store the result to makeCacheMatrix
## The environment of makeCacheMatrix works as a cache for cacheSolve

cacheSolve <- function(x, ...) {
        ## get inv from cache
        inv <- x$getinv()
        ## if inv values NULL, then return the inv directly
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## otherwise, calculate the inverse matrix
        data <- x$get()
        inv <- solve(data)
        ## save the value of inv into the environment of cacheSolve
        x$setinv(inv)
        ## Return a matrix that is the inverse of 'x'
        inv
}
