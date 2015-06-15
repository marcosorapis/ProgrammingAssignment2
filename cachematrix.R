## The two following functions allow to cache the inverse of a matrix in order to save 
## time and avoid to computed it repeatedly


# makeCacheMatrix is a function that stores a list of functions
# makeCacheMatrix contains 4 functions: set, get, setinv, getinv
# get is a function that returns the matrix x stored in the main function
# set is a function that changes the matrix stored in the main function
# getinv is a function that returns the inverse matrix inv stored in the main function
# setinv is a function that changes the inverse matrix inv stored in the main function
makeCacheMatrix <- function(x = matrix()) {
  inv <- matrix()
  set <- function(y) {
    x <<- y
    inv <<- matrix()
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
}


#cacheSolve verifies that the value inv, stored previously with getinv, exists 
#and is not NULL. If it exists in memory, it simply returns a message and the 
#value inv, that is supposed to be the inverse function. Otherwise it calculates 
#the inverse of the matrix and x$setinv(inv) stores it in the object generated 
#assigned with makeCacheMatrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv  
}