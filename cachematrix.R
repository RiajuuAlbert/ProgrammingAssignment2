## Week 3: Programming Assignment 2

## Put comments here that give an overall description of what your
## functions do

## Define the argument with default mode of "matrix"
makeCacheMatrix <- function(x = matrix()) { 
  invCache <- NULL                          ## Initialize invCache as NULL that will hold value of an inversed matrix 
  set <- function(y) {                      ## Define the function to assign a new value of matrix
    x <<- y                                 ## in parent environment (denoted by "<<-")
    invCache <<- NULL                       ## If there is a new matrix, reset invCache to NULL
  }
  get <- function() x                       ## Define the function to return value of the matrix argument (get)
  
  setInverse <- function(invCache) invCache <<- invCache  ## Assigns value of invCache in parent environment
  getInverse <- function() invCache                       ## Gets the value of invCache if called
  list(set = set, get = get, setInv = setInverse,  
              getInv = getInverse)                       
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invCache <- x$getInv()
  if(!is.null(invCache)) {        ## If there is an inverted matrix cache, the function will
    message("Cache is being acquired, please wait...")## access the matrix from the cache to save time and 
    return(invCache)              ## processing power
  }
  data <- x$get()
  invCache <- solve(data, ...)
  x$setInv(invCache)
  invCache
}