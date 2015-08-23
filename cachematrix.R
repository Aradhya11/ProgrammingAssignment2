## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates and returns a list of functions
## used by cacheSolve to get or set the inverted matrix in cache

makeCacheMatrix <- function(x = matrix()) {
# stores the cached value
# initialize to NULL
inv <- NULL

# create the matrix in the working environment
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # get the value of the matrix
    get <- function() x
    
    # invert the matrix and store in cache
    setinverse <- function(inverse) inv <<- inverse
    
    # get the inverted matrix from cache
    getinverse <- function() inv
   
    # return the created functions to the working environment
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve calcluates the inverse of the matrix created in makeCacheMatrix
## If the inverted matrix does not exist in cache,
## it it created in the working environment and it's inverted value
## is stored in cache

cacheSolve <- function(x, ...) {
    ## attempt to get the inverse of the matrix stored in cache
        inv <- x$getinverse()
    
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
   # set and return inverse of matrix
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
