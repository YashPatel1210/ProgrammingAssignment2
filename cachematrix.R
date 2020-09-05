## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function create Matrix inversion if not in cache.

makeCacheMatrix <- function(x = matrix()) {
  
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinver <- function(solve) s <<- solve
  getinver <- function() s
  list(set = set, get = get,
       setinver = setinver,
       getinver = getinver)

}


## Write a short comment describing this function
## this function create a cache of new matrix inversion.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  s <- x$getinver()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinver(s)
  s
}
