## These 2 functions are written for the Coding Assignment 2 of week 3
## of the course "R programming" on Coursera
## GitHub github.com/RonnyRuettimann


## This function generates a special matrix object that can cache its inverse

makeCacheMatrix <- function(mat = matrix()) { # input n x n matrix, assumption: invertible
  ## Create a matrix object to use with the function cacheSolve
  
  inv <- NULL # Initialise the inverse. This will hold the value of the (cached) inverse
  
  # This functions sets the matrix and deletes any inverse
  set <- function(n){
    mat <<- n     # assign the new value of the matrix in the parent environment
    inv <<- NULL  # reset inv to null if there is a new matrix
  }
  
  # This functions retrieves the matrix, returns the matrix
  get <- function() mat
  
  # This function sets the inverse in inv
  setInv <- function(inverse) inv <<- inverse #assigns the value of the inverse in the parent environment
  
  #This function retrieves the inverse, returns the inverse
  getInv <- function() inv
  
  # return a list returning the four functions set, get setInv, getInv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## This function computes the inverse of the matrix as defined above by makeCacheMatrix
## If the inverse was already computed, the function retrieves the cached inverse

cacheSolve <- function(mat, ...) {
  ## Return a matrix that is the inverse of 'mat'
  ## input needs to be of the form as generated with the fct makeCacheMatrix
  inv <- mat$getInv() # get the inverse of mat, if non-existant, is NULL
  if (!is.null(inv)) {                # if the inverse of the matrix was already computed,
    message("getting cached data")  # print message and
    return(inv)                     # return the cached inverse
  }
  # if the inverse of the matrix was not calculated before
  data <- mat$get() # get the matrix
  inv <- solve(data, ...) # compute the inverse
  mat$setInv(inv) # set the inverse in the matrix object
  inv # return the computed inverse
  
}
