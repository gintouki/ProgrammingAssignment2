## Functions to implement matrix inverse with cached value
## To be aware: These functions does not check if the matrix supplied
##              is in fact invertible, neither it checks if it is of
##              correct size (i.e. square)

## This code implements caching
## Returns a "list" object with following methods:
## set.matrix - caches the initial matrix object.
##      arguments: x - object of class "matrix"
##      returns: NULL
## get.matrix - returns cached matrix
## set.inverse - caches inverse matrix
##      arguments: x - inverse matrix
##      returns: provided argument
## get.inverse - returns cached inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse_m <- NULL # Initialize our cache variable
  
  #Implement object methods
  set.matrix <- function(y) {
    x <<- y
    inverse_m <<- NULL # Empty cache if matrix was modified
  }
  get.matrix <- function() x
  set.inverse <- function(inv) inverse_m <<- inv
  get.inverse <- function() inverse_m
  list(set.matrix = set.matrix, get.matrix = get.matrix,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
}


## This code finds inverse matrix if none was cached
## or returns the cached version.
##      arguments: x - object of class "list" created by 'makeCacheMatrix'
##      returns: object of class "matrix"
cacheSolve <- function(x, ...) {
  im <- x$get.inverse()
  if(!is.null(im)) { # Check if cache is empty
    message("getting cached data")
    return(im)
  }
  data <- x$get.matrix() # If no cached object, receive initial matrix 
  im <- solve(data, ...) # and compute the inverse.
  x$set.inverse(im)      # Cache computed matrix.
  im
}
