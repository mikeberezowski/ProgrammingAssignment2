## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#makeCacheMatrix creates a object vector which stores a matrix and it's inverse
#USAGE: cm <- makeCacheMatrix() creates the vector object cm
# cm$get() gets the matrix
# cm$set() sets the matrix
# cm$getinverse() gets the cached inverse of the matrix
# cm$setinvers() sets the inverse of the matrix
# cm$list() lists the available methods
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
#a function to calculate the inverse of a matrix on a cacheMatrix object
#USAGE: cacheSolve(cm)
#ARGUMENT: x : a CacheMatrix object vector
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
