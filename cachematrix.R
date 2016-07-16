# creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  myinverse <- NULL
  set <- function(y) {
    x <<- y
    myinverse <<- NULL
  }
  get <- function() x
  setit <- function(toinverse) myinverse <<- toinverse
  getit <- function() myinverse
  list(set=set, get=get, setit=setit, getit=getit)
}

# computes inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
  myinverse <- x$getit()
  if(!is.null(myinverse)) {
    message("getting cached data")
    return(myinverse)
  }
  data <- x$get()
  myinverse <- solve(data)
  x$getit(myinverse)
  myinverse
}