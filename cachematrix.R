## The main function (makeCacheMatrix) is a list of functions to set, get and inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  invCached <- NULL ## initialize inverse
  
  ## set x in parent env with the desired value, if inverse is already set, get rid of it!
  set <- function(inputValue = matrix()) {
    x <<- inputValue 
    invCached <<- NULL
  }
  
  get <- function() x
  
  ##set inverse variable in parent env to desired value and return the value as a convenience
  setInverse <- function(invVal) {
    cachedInv <<- invVal 
    return(invCached)
  }
  
  getInverse  <- function() invCached
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## given the list variable from the first function, will first check to see if there's already a cached inverse and return
## otherwise will attempt to solve its inverse and set/return it

cacheSolve <- function(x=makeCacheMatrix(1:4, nrow=2, ncol=2), ...) { ##special matrix provided or create a test 2x2 matrix
  
  ## let's see if there's something there already
  calculatedInverse <- x$getInverse() 
  
  ##check if there's a cached value AND it's a matrix
  if(!is.null(calculatedInverse) && is.matrix(calculatedInverse)) { 
    message("We found cached data and saved valuable cpus!!!")
    return(calculatedInverse)
  }
  
  ## otherwise get the matrix
  matrixToSolve <- x$get()  
  
  ## try to solve the matrix and catch errors and warnings
  calculatedInverse <- tryCatch({ 
    solve(matrixToSolve)
  }, warning=function(w) {
    message("This may not be the result you're looking for")
    message(w)
  }, error=function(e) {
    message("Something went wrong solving your matrix")
    message(e)
    message("\n")
  })
  
  ## whatever the case, set the value of the inverse (NULL if something went wrong)
  message("Setting the value of inverse to:") 
  x$setInverse(calculatedInverse)
}