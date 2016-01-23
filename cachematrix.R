#Creates a function to cache the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
#Creates a function that checks whether the inverse of the matrix has already been cached, and if it has, returns the 
#cached inverse from the function above. If it hasnt, it calculates the inverse of the matrix.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  #if the matrix inverse has already been cached, returns "Getting cached data" and then returns the cached value from the 
  #function above.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
