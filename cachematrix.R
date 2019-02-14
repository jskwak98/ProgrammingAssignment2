## Put comments here that give an overall description of what your
## functions do
------------------------------------------------------------------------------
# The first function, makeCacheMatrix, makes a list of functions that
 
# 1. set the matrix
# 2. get the matrix
# 3. set the value of inversed matrix
# 4. get the value of inversed matrix
 
# When the second function, cacheSolve get the list which is
# produced by the first function as parameter,
# it will check if the list contains cached value of inversed matrix.
# If there's no cached value, then it'll do the computation and then cache it.
# If cached value exists, the value will be returned without computation.
------------------------------------------------------------------------------

  
## Write a short comment describing this function
------------------------------------------------------------------------------
# I is the variable that store the inversed matrix of x.
# I've already described about each function above in the order
# that they appear.
------------------------------------------------------------------------------
  
makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) I <<- solve
  getSolve <- function() I
  list(set = set, get = get, 
       setSolve = setSolve,
       getSolve = getSolve)
}


## Write a short comment describing this function
------------------------------------------------------------------------------
# I gets the cached value from the list that's made by
# makeCacheMatrix function. Then the existence of cached value
# is checked. If cached value doesn't exist, it does the computation
# by solve(x) and cache the result.
------------------------------------------------------------------------------
  
cacheSolve <- function(x, ...) {
  I <- x$getSolve()
  if(!is.null(I)){
    message("getting cached matrix")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setSolve(I)
  I
}