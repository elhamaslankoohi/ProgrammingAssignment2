## This code will store the results of a inveritable matrix inversion that can speed up the program

## makeCacheMatrix is a function that generate a list of function that set the value of matrix (set), get the value of matrix (get), 
##set the inverse of the function(setInverse), get the inverse of the function (getInverse). 


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## caheSolve compute the inverse of an invertible matrix, however if the values has already been computed the function skips 
## recomputation and shows the message "getting cached data" 

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
