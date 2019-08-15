#Week 3 programming assignment
#Chelsea Lutz

## Function 1 creates a special "matrix" object that can cache its inverse
## Function 2 computes the inverse of the special "matrix" returned by Function 1

## Create makeCacheMatrix to cache inverse

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y){
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setinver <- function(solveMatrix) inver <<- solveMatrix
  getinver <- function() inver
  list(set = set, get = get, setinver = setinver, getinver = getinver)
}


## Compute inverse of matrix returned in createCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inver <- x$getinver()
  if(!is.null(inver)){
    return(inver)
  }
  data <- x$get()
  inver <- solve(data)
  x$setinver(inver)
  inver 
}
