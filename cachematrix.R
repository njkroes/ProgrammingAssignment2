## These 2 functions create a special "matrix" object and cache the inverse
## when computed to avoid multiple computations

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # Instantiate inverse as null  
  xinverse <- NULL
  
  # functions to get, set, set inverse and get inverse of matrix
  get <- function() x
  set <- function(y) {
    x <<- y
    xinverse <<- NULL
  }
  setinverse <- function(inverse) xinverse <<- inverse
  getinverse <- function() xinverse
  
  # return list containing functions
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
}


## This function computes computes the inverse of a given special matrix
## from the cache if possible
cacheSolve <- function(x, ...) {
  # Gets current inverse for matrix
  inverse <- x$getinverse()
  
  # Check if null
  if (!is.null(inverse)){
    
    # Return cached inverse
    message("getting cached data")
    return(inverse)
  }
  
  # Get matrix
  data <- x$get()
  
  # Invert matrix
  inverse <- solve(data)
  
  # Cache inverse
  x$setinverse(inverse)
  
  # Return inverse
  inverse
}