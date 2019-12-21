## this function creates a matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) { ##define the arguement
  inv <- NULL ##initialize inv as null
  set <- function(y) { ##define the set function
    x <<- y ##value of matrix in parent environment
    inv <<- NULL
  }
  get <- function() x ##define the get function, return value of the matrix arguement
  setinverse <- function(inverse) inv <<- inverse ##assign value of inv in parent environment
  getinverse <- function() inv ## get the value of inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## this function calculate the inverse of the matrix above
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}