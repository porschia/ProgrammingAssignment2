## The "makeCacheMatrix" and "cacheSolve" functions take an invertible
## matrix, store or "cache" it, invert it and return it

## "makeCacheMatrix" takes a matrix as an argument and initiates a 
## variable with functions that can set and get the original matrix
## and also set and get the inverted matrix

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


## "cacheSolve" takes a variable that has been initiated by
## "makeCacheMatrix", checks to see if a matrix has been
## stored and if it has it returns it. If there is no stored
## matrix it inverts the matrix passed in and caches the
## inverted matrix

cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
