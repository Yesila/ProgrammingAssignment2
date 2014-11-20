## Put comments here that give an overall description of what your
## functions do

## Creates a 'matrix' object, that contains functions to set and retrive 
## the matrix itself and it's inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
    }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## takes an object of 'makeCacheMatrix' type and retrieves a set inverse 
## if it exists.  If it does not exist it first computes the inverse then
## sets the inverse before returning it.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)     ## if inverse already exists return it
    }
  matrix <- x$get()
  inverse <- solve(matrix, ...)    ## computes inverse of 'x'
  x$setinverse(inverse)            ## sets inverse of 'x'
  inverse        ## Return the matrix that is the inverse of 'x'
}
