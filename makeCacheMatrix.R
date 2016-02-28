### The inverse of x is calculated.
### Assume x is invertible.
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

#### This function computes the inverse matrix given for makeCacheMatrix above. 
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m) ### verific If the inverse has already been calculated. 
  }
  data <- x$get()
  m <- solve(data, ...) ### The inverse is calculated if this not do yet.
  x$setinverse(m)
  m
}
