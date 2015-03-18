## Two functions create a matrix object and solve for the inverse of the matrix


## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      
      x <<- y
      m <<- NULL      
    }
    
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function () m
    list(set=set, get=get,
         setinverse=setinverse,
         getinverse=getinverse)

}


## computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setinverse(m)
  m
}
