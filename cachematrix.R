## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix sets, stores a matrix and uses that matrix to be inversed

makeCacheMatrix <- function(x = matrix()) {
  # Initialize the matrix and inverse variables
  mat <- NULL
  inv <- NULL
  
  # Function to set the matrix and calculate its inverse
  set <- function(matrix) {
    mat <<- x
    inv <<- NULL
  }
  
  # Function to get the cached matrix
  get <- function() {
    mat
  }
  
  # Function to get the cached inverse if available, otherwise calculate and cache it
  getInverse <- function() {
    if (!is.null(inv)) {
      message("Getting cached inverse.")
      return(inv)
    } else {
      message("Calculating inverse and caching.")
      inv <<- solve(mat)
      return(inv)
    }
  }
  
  # Return a list of functions for manipulating the matrix object
  list(setMatrix = set, getMatrix = get, getInverse = getInverse)
}
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  # Check if the matrix is available in the cache
  matrix <- x$getMatrix()
  
  # If matrix is NULL, return an error
  if (is.null(matrix)) {
    stop("Matrix not set. Use setMatrix() to set the matrix.")
  }
  
  # Get the cached inverse or calculate and cache the inverse using getInverse()
  inverse <- cacheMatrix$getInverse()
  
  # Return the inverse matrix
  inverse
}

#Example
cacheMatrix <- makeCacheMatrix()

cacheMatrix$setMatrix(matrix(c(4, 3, 2, 1), nrow = 2))

inverse <- cacheSolve(cacheMatrix)

print("Inverse Matrix:")
print(inverse)
