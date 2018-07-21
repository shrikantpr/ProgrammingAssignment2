## makeCacheMatrix function creates a list of functions 
## set, get, setInv and getInv. The arguement to the makeCacheMatrix
##function is an inversible matrix.

## Takes an inversible matrix as an arguement and creates a list of functions

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  get <- function() x
  setInv <- function(Invrs) Inv <<- Invrs
  getInv <- function() Inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Returns the inverse of a matrix from cache if already calculated earlier
## else calcultes the inverse of a matrix saves it in cache and returns the inverse of matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Inv <- x$getInv()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  A <- x$get()
  Inv <- solve(A)
  x$setInv(Inv)
  Inv
}
