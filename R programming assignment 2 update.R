## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.
## This function creates a special "matrix" object that can cache its inverse.

library(MASS)
makeCacheMatrix <- function(x = matrix()) 
  {
  inv <- NULL               ##initializing inverse as NULL
  set <- function(y) {
                       x <<- y
                       inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse)inv<<-inverse
  getinv <- function() {
                                inver<-ginv(x)
                                inver%*%x
                           }
      list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}



## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.


cacheSolve <- function(x, ...)  ##gets cache data 
  {
  inv <- x$getinv()
  if (!is.null(inv))  {             ##checking whether inverse is NULL                  
                       message("getting cached data")
                       return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv                    ## returns a matrix that is a inverse of "x"
}