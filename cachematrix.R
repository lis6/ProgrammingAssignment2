## function calculate the inverse of a square matrix, and
## cache the result for later use. If the same matrix appeared 
## again, the inverse matrix is get from the cache rather than 
## to be calculated again

## function creates a special "matrix" object that can cache its 
## inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list (set = set, get = get, setinverse = setinverse,
        getinverse = getinverse)

}


## function computes the inverse of the matrix "x" returned by 
## makeCacheMatrix above. If the inverse has already been in the 
## cache, it will be simply retrieved from the cache rather than
## calculated again.
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
