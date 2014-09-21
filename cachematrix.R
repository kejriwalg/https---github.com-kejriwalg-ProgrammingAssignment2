## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  cachematrix <- NULL
  set <- function(y) {
    x <<- y
    cachematrix <<- NULL
  }
  
  get <- function() x
  setmatrix <- function(x) cachematrix <<- solve(x)
  getmatrix <- function() cachematrix
  
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
  
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  data <- x$getmatrix()
  if(!is.null(data)) {
    message("getting cached data")
    return(data)
  }
  
   
  ## get the matrix
  data <- x$get()
  
  ## Inverse the matrix
  data <- x$setmatrix(data)
  
  return(data)
  
}