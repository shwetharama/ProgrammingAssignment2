## Creates a special object - matrix that can cache its inverse

## Sets the value of the matrix
## Gets the value of the matrix
## Sets the value of inverse
## Gets the value of inversr

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  ser <- function(y) {
    x <<-y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}



## The following function calculates the mean of the special "matrix" created with the above function.
## It first checks to see if the inverse has already been calculated. If so, it gets it from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the solve function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
