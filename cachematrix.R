## Coursera R-Programming assignment 2 - week 3
## Caching the Inverse of Matrix
## Matrix supplied to this function should be always invertible

## "makeCacheMatrix" function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(mat = matrix()) {
  #function variable "mat" sets to an empty matrix
  
  # Assigns inverse value to NULL
  inv <- NULL
  
  # set function sets matrix to a new matrix(y) & reset inverse matrix to NULL, if any pre-calculated value
  set <- function(y) {
    mat <<- y
    inv <<- NULL
  }
  
  #returns the matrix "mat"
  get <- function() mat
  
  #Sets the inverse matrix to inv
  setinverse <- function(solve) inv <<- solve
  
  #returns the inverse matrix inv
  getinverse <- function() inv
  
  #returns the list of above defined values
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## "cachesolve" function computes the inverse of special matrix returned by "makeCacheMatrix
## Matrix supplied to this function should be always invertible
## If the matrix is same & inverse has been already calculated, this will retrive from cache,
## else it will calculate a new inverse & cache it

cacheSolve <- function(mat, ...) {
  ## Return a matrix that is the inverse of 'mat'
  
  #retrives a inverse matrix
  inv <- mat$getinverse()
  #Check if it is NULL. & returns a valid cache, if not NULL
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  #Calculates a new inverse & returns it
  data <- mat$get()
  inv <- solve(data, ...)
  mat$setinverse(inv)
  inv
}