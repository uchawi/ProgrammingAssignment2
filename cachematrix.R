## Function makeCacheMatrix, put matrix to cache, so:
## set and get value of matrix into cache
## set and get the value of inverse matrix using function solve()
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}
## Function check if inverse matrix is already computated and set in cache, if so it is get from cached, if no it is compute
cacheSolve <- function(x, ...) {
  ## Return  matrix that is the inverse of 'x'
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
