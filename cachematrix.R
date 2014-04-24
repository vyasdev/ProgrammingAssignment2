## This is a pair of functions, one is a special matrix object that can
## cache its inverse. The other checks if the inverse is available in the 
## cache, if yes, it retrieves it, else it calculates the inverse and passed 
##back to the first function to store in cache

## The makeCacheMatrix function takes a matrix as an argument 
##and caches its inverse in memory

makeCacheMatrix <- function(x = matrix()) { ##Input matrix as argument.
  inv <- NULL                               ##Defines inverse as NULL.
  set <- function(y) {                      ##Sets next matrix as the 
    x <<- y                                 ##function argument.
    inv <<- NULL
  }
  get <- function() x                             ##Function to get the matrix.
  setInverse <- function(inverse) inv <<- inverse ##Function to set the inverse
  getInverse <- function() inv                    ##Function to retrieve the inverse
  list(set = set, get = get,                      ##Function returns 4 List items,
       setInverse = setInverse,                   ##List is actually 4 functions wrapped in a list
       getInverse = getInverse)
}


## cacheSolve works on the results of makeCacheMatrix. It checks if 
##the inverse to the called matrix is available in Cache. If yes, it 
##just retrieves the result. Else it calculates the inverse and 
##sets the inverse in cache.

cacheSolve <- function(x, ...) {    ##Input matrix as argument.
  inv <- x$getInverse()             ##Query the x matrix's cache. 
  if(!is.null(inv)) {               ##Test if there is a cache.
    message("Getting cached data")  
    return(inv)                     ##Just return the cache, no computation needed.
  }
  data <- x$get()                   ##If there's no cache, get the matrix,
  inverse <- solve(data)            ##and we compute its inverse here
  x$setInverse(inverse)             ##Set the new inverse also in cache.
  inverse                           ## Return a matrix that is the inverse of 'x'
}
