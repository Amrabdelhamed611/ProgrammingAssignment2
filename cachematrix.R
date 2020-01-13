# This function computes the inverse of the special "matrix" returned by 
#   makeCacheMatrix above. If the inverse has already been calculated (and the matrix  
#   has notchanged), then cacheSolve should retrieve the inverse from the cache.
makeCacheMatrix <- function(x = matrix()) {
  # makes a cache matrix from a given matrix
  # 1. initialize the cache Matrix 'cacheMatrix'
  # assign the value NULL for the first initialization
    inv <- NULL
  # 2. define the method named 'set'
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # 3. define the method named 'get'
  # return the matrix 'x'
  get <- function() x
  # 4. define the method named 'setinv'
  setinv <- function(inverse) inv <<- inverse
  # 4. define the method named 'getinv'
  # that will return the cached inverse of 'x'
  getinv<- function() inv
  # 6. list the names of all methods that will be known to the outside world
  list(set= set,
       get = get,
       setinv = setinv,
       getinv = getinv)

} 

# 'cacheSolve'
# return the inverse of a given matrix utilizing the cache

cacheSolve <- function(x, ...) {

  # 1. check the content of cache matrix
  invMatrix <- x$getinv()
  # 2. if the content is not null then: return the result
  if (!is.null(invMatrix)) {
    message("loading cache matrix...")
    return(invMatrix)
  }
  # 3. if the content is empty then: 
  # get the matrix, create, set, update and return the cache matrix
  else {
    dMatrix <- x$get()
    invMatrix <- solve(dMatrix, ...)
    x$setinv(invMatrix)
    return(cacheMatrix)
  }
}
