## Assume that the matrix supplied is invertible, the first function creates a special matrix object that can cache its inverse.
## The second function computes the inverse of the special matrix returned by the first function. 
## If the inverse has already been calculated (and matrix has not changed), the second function should retrieve the inverse from the cache.

## The makeCacheMatrix function creates a special matrix, which is a list containing four functions
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse matrix
## 4. Get the value of the inverse matrix

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


## The cacheSolve calculates the inverse of the special matrix created with the above function.
## It first check to see if the inverse has already been calculated, if so gets the cache of the inverse.
## Otherwise, it will compute the inverse of the matrix and set value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}

