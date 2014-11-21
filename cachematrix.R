## Programming Assignment 2 - Caching the Inverse of a Matrix
## Create a pair of functions that cache the inverse of a matrix

## The first function will return a list containing methods to set the 
## matrix, get the matrix, set the inverse of the martix, and get the inverse
## of the matrix
## takes a matrix as its parameter

makeCacheMatrix <- function(x = matrix() ) {
  
  ## clear cache and create set/get functions
  inv <- NULL
  
  ## Method to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Method the get the matrix
  get <- function() x
  
  ## Method to set the inverse of the matrix
  setInverse <- function(xinv) inv <<- xinv
  
  ## Method to get the inverse of the matrix
  getInverse <- function() inv
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## cacheSolve() takes a matrix cache (produced by makeCacheMatrix()) and
## returns either a cached inverse or a newly calculated one

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'm' 
  inv <- x$getInverse()
  
  ## Return the inverse if it has already set
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  message("calculating inverse")
  data <- x$get()
  inv <- solve(data, ...)
  
  ## Set the inverse to the object
  x$setInverse(inv)
  
  ## Return the matrix that is the inverse
  inv
}