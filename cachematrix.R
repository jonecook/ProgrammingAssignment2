## These funtions are meant to satisfy the requirements for 
#Assignment 2

#The first function, makeCacheMatrix creates a special "matrix", which is
#really a list containing functions to

#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  #this sets the stored value of the matrix and sets the inverse to NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #this returns the stored value of the matrix
  get <- function() x
  #this sets the inverse of the matrix
  setinverse <- function(solve) m <<- solve
  #this returns the stored value of the inverse of the matrix
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


#The following function calculates the inverse of the special 
#"matrix" created with the above function. However, it first checks 
#to see if the inverse has already been calculated. If so, it gets 
#the inverse from the cache and skips the computation. Otherwise, it 
#calculates the inverse of the data and sets the value of the 
#inverse in the cache via the setinverse function.

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  #check if the inverse value is already stored and if so, write a 
  #message and return the value
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #otherwise, calculate the inverse and store it
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
