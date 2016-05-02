## Cache Matrix script for coursera.org r programming class
## The following will cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  ## set a null value for matinv
  matinv <- NULL
  
  ##set the value of the matrix
  set <- function(y) {
    x <<- y
    matinv <<- NULL
  }
  
  ##get the value of the matrix
  get <- function() x
  
  #set the value of the inverse matrix
  setinv = function(inverse) matinv <<- inverse
  
  ## get the value of the inverse matrix
  getinv = function() matinv
  list(set=set, get=get, setinv= setinv, getinv=getinv)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matinv <- x$getinv
  
  # if there's something in matinv
  if(!is.null(matinv)) {
    message("getting the cached data")
    return(matinv)
  }
  ## if not - find the inverse
  data <- x$get()
  matinv = solve(data, ...)
  x$setinv(matinv)
  return(matinv)
}