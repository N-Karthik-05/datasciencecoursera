## makeCacheMatrix creates a special matrix, it is a list containing a function to
##1. set the value of the matrix, 
##2. Get the value of the matrix,
##3. Set the value of the inverse of the matrix, 
##4. Get the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  

}


## cacheasolve function calculates the inverse of the special matrix returned
## by makeCacheMatrix, if the inverse is already calculated it will not calculate,
## the inverse  again.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
        ## Return a matrix that is the inverse of 'x'
}
