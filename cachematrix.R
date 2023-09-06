install.packages('MASS')
library('MASS')

## Put comments here that give an overall description of what your
## functions do

##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setInverse <- function(inverse) {
    inv <<- solve(x)
  }
  
  getInverse <- function() {
    inv
  }
  
  list(set = set,          # gives the name 'set' to the set() function defined above
       get = get,          # gives the name 'get' to the get() function defined above
       setInverse = setInverse,  # gives the name 'setmean' to the setmean() function defined above
       getInverse = getInverse)  # gives the name 'getmean' to the getmean() function defined above
}


## This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then cacheSolve should retrieve the 
# inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  m <- inverse(data, ...)
  x$setInverse(inv)
  return(inv)        
}







# makeVector <- function(x = numeric()) {
#   m <- NULL
#   
#   set <- function(y) {
#     x <<- y
#     m <<- NULL
#   }
#   
#   get <- function() {
#     x
#   }
#   
#   setmean <- function(mean) {
#     m <<- mean
#   }
#  
#    getmean <- function() {
#     m
#   }
#   
#    list(set = set,          # gives the name 'set' to the set() function defined above
#         get = get,          # gives the name 'get' to the get() function defined above
#         setmean = setmean,  # gives the name 'setmean' to the setmean() function defined above
#         getmean = getmean)  # gives the name 'getmean' to the getmean() function defined above
# }
# 
# 
# cachemean <- function(x, ...) {
#   m <- x$getmean()
#   if(!is.null(m)) {
#     message("getting cached data")
#     return(m)
#   }
#   data <- x$get()
#   m <- mean(data, ...)
#   x$setmean(m)
#   m
# }
# 
