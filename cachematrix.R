# MakeCacheMAtrix is a function used to create of object of matrix class
## and its associated methods
##  - set(), get() can be called directly
##  - setinv(), getinv(), should be alled via the cacheSolve method as
##     matrix inversion operation is implemented in this method


## set() - instantiate the matrix with value
## get() - retrieve the matrix
## setinv() - accept the inverse matrix as parameter and store in objects created by makeCacheMatrix,
##            inverse matrix is derived in the CacheSolve function
## getinv() - retrieve the stored inverse matrix

# how to use this function
## source("cachematrix.R")    -- source this file
## a <- makeCacheMatrix()     -- create an object a of class matrix
## a$set(1:4,2,2)             -- eg instantiate a 2x2 matrix of value from 1 to 4
## a$get()                    -- get the value of the matrix
## cacheSolve(a)              -- return an inverse matrix
## a$get() %*% cacheSolve(a)  -- should return an identity matrix to prove that the cacheSolve() function
##                               does create an inverse matrix

##  
##

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y, r,c) {
    x <<- matrix(y, r,c )
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv)  m <<- inv
  
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
  

## cacheSolve accept an matrix object as parameter
## Using the objects, it accessed to see if the inverse matrix is created.
## if yes, it return this object, otherwise it computes the inverse matrix, store
## it in the "calling" matrix and also return the inverse matrix values

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  
  x$setinv(m)
  m
}
