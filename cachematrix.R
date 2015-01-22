## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function is to computer the inverse of a matrix (assuming given matrix is always 
## invertable). It will first check if the inverse has been computed previously.It will retrieve
## the results from the cache if it is already there.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
  x <<- y
  m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) m <<- solve(x)
  getinverse <- function() m
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
