### Coursera: R Programming, Week 3 - Programming Assignment 2: Lexical Scoping

## The aim of the following two functions ('makeCacheMatrix' and 'cacheSolve') is to utilise the R scoping environment (lexical scoping)
## to cache potentially time-consuming computations, by preserving the state inside an R object. In particular, these functions will
## cache the inverse of a matrix.

### The 'makeCacheMatrix' function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {     # set the matrix
    x <<- y                # assigns 'x' the value 'y', only in this environment
    inv <<- NULL           # assigns 'inv' the value 'NULL', only in this environment
  }
  get <- function() x      # get the matrix
  setinverse <- function(solve) inv <<- solve   # set the matrix inverse - 'solve' returns the inverse of a square invertible matrix
  getinverse <- function() inv                  # get the matrix inverse
  list(set = set, get = get,                    # return the matrix inverse
       setinverse = setinverse,
       getinverse = getinverse)
}



### The 'cacheSolve' function computes the inverse of the special "matrix" returned by the 'makeCacheMatrix' function.
### If the inverse has already been calculated (and the matrix has not changed), then `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()   # checks if the matrix inverse has already been calculated
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)           # return the matrix inverse calculated using 'makeCacheMatrix'
  }
  data <- x$get()           # if the matrix inverse has not already been calculated, get the matrix
  inv <- solve(data, ...)   # get the matrix inverse
  x$setinverse(inv)         # set the matrix inverse
  inv                       # return the matrix inverse
}
