## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#makeCacheMatrix creates a 'matrix' object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) 

{
  
  #m is cached, initially set to null for any new value
  m <- NULL
  
  #gets and sets
  set <- function(y) {
    x <<- y
    
    m <<- NULL
  }
  
  # returns the value of the original vector
  get <- function() x 
  
  #called by cacheSolve during 1st cacheSolve
  setMatrix <- function(matrix) m <<- x 
  
  #returns cached value to cacheSolve on subsequent access
  getMatrix <- function() m 
  
  #accessed each time makeCacheMatrix is called
  list(set = set, get = get, 
       setMatrix = setMatrix,
       getMatrix = getMatrix)
  


}


## Write a short comment describing this function
## Returns a matrix that is the inverse of 'x'
#computes the inverse of a special 'matrix' returned by makeCacheMatrix

cacheSolve <- function(x, ...) 
  
{
   
  
  # if the inverse has not been calculated, cacheSovle calculates it
  # stores it in cache and returns it
  # if the inverse was calculated earlier, cacheSolve 
  #returns the value created by makeCacheMatrix
  
  #gets the matrixInverse from object matrix
  m <- x$getMatrix() 
  if(!is.null(m)) 
  {
    message("getting cached data")
    return(m)
  }
  
  #if inverse has not been cached, calculates the matrix inverse
  #caches and returns it
  data <- x$get()
  m <- solve(data, ...)
  x$setMatrix(m)
  m

}
