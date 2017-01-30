## This set of two function is used to calculate inverse matrix and in order to perform
## costly computations faster it caches the inverse of a matrix rather than compute it 
## repeatedly.

## Function "makeCacheMatrix" creates a list of function, which can set and get the value 
## of matrix, and also set and get the value of inversion matrix. The function is used as
## an input for "cacheSolve" function.
## Input: x - square invertable matrix.
## Output: list of functions set the matrix, get the matrix, set the inverse matrix,
## get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<-y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv_value) inv <<- inv_value
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Function "cacheSolve" calculates the inverse matrix, but before calculating it, it 
## checks if the inverse has already been calculated. In that's case it returns cached
## value and skips the computation. If not, it calculates the inverse matrix, and sets  
## the result in the cache via setinv function
## Input: output of "makeCacheMatrix()"
## Output: inverse of origin matrix, which was used as an input for "makeCacheMatrix()"

cacheSolve <- function(x, ...) {
   inv <- x$getinv()
   
   ## check cache for the inverse matrix
   if (!is.null(inv)) {
     message("Getting cached data")
     return(inv)
     }
  
   ## if not in cache, calculate the inverse matrix
   matrix_data <- x$get()
   inv <- solve(matrix_data, ...)
   
   ## cache and return the inverse matrix
   x$setinv(inv)
   return(inv)
}
