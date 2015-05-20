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
  ## function to retrieve the original matrix submitted
  get <- function() x
  ## function used by cacheSolve to store the inverse in m
  setinverse <- function(inverse) m <<- inverse
  ## function to retrieve the inverse
  getinverse <- function() m
  ## list that is returned with functions assigned to names
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
  ## retrieve matrix inverse if present
  m <- x$getinverse()
  ## check to see if inverse is present then return it
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  ## retrieve original matrix and store it in a variable
  data <- x$get()
  ## create the inverse of the matrix and store it in m
  m <- solve(data, ...)
  ## call setinverse to store m in all environments
  x$setinverse(m)
  m
}
