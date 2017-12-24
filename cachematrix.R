## The following pair of functions will cache the inverse of a matrix
##if it has been computed already to avoid repeating its inverse computation(which is costly)
##It will compute the inverse of the matrix if it has not been computed yet

## makeCacheMatrix function creates a special "matrix" object that 
##can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


##cacheSolve functions computes the inverse of the special matrix 
##produced/returned by makeCacheMatrix function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
