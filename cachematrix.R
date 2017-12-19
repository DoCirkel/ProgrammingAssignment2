## Put comments here that give an overall description of what your
## functions do

## This function first: Sets the matrix, then gets the matrix, sets the inverse and gets the inverse. 


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function checks if the inverse has already been calculated. 
# If so, it returns a message and the inverse.
# If not, it calculates the inverse, gives this value to the cache and returns the inverse


cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  return(inv)
}

