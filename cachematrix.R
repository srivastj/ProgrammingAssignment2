## Inverse Matrix operations are costly, so if we once compute the inverse, we must cache the result for next query.
## makeCacheMatrix : Function would make a format to represent matrix, it would have getter and setter methods for both matrix data and Inverse of Matrix.


## Function take input as matix and exposes getter and setter for matrix and for computing inverse of matrix.
## set method, assigns new matrix data to x and also NULL to I.
## get method, retrives the matrix data.
## setinverse method, would set the new inverse of matrix. 
## getinverse mehtod, would retrive the inverse of matrix.
makeCacheMatrix <- function(x = matrix()) {
  
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) I <<- inverse
  getinverse <- function() I
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## CacheSolve : would take a makeCacheMatrix() as an input. 
## if getinverse() is not null, we return the cache value.
## else, we compute the result and store it my calling x$setinverse method. 
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  I <- x$getinverse()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setinverse(I)
  I
}


