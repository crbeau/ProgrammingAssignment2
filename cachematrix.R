## Matrix inversion is usually a costly computation; therefore, there may be some benefit to caching it for repeated use, 
## instead of re-computing it repeatedly.  To aid on this endeavor, below are a pair of functions that, collectively,   
## computes the inverse of the matrix and stores it in a cache for future use, without having to repeat the calculations 
## every time it is needed.  For this exercise, the matrix is always assumed to be square and invertible.

## This function "makeCacheMatrix" creates the cache "matrix" that will be used to service the pertinent cache operations

makeCacheMatrix <- function(x = matrix()) {
  mymatrixinv <- NULL                                            ## Initializing object mymatrixinv (which will contain the
  ## inverse to NULL.  A test for the NULL is made on the cacheSolve
  ## function, to determine if it has already been computed.
  setmymatrix <- function(y) {
    x <<- y
    mymatrixinv <<- NULL
  }                                                              ## Function "setmymatrix" creates the cache matrix
  getmymatrix <- function() x                                    ## Function "getmymatrix" retrieves the matrix to be inverted
  setmymatrixinv <- function(minv) mymatrixinv <<- minv          ## Function "setmymatrixinv" sets the matrix inverse on the cache
  getmymatrixinv <- function() mymatrixinv                       ## Function "getmymatrixinv" retrieves the inverse from the cache
  list(setmymatrix = setmymatrix, getmymatrix = getmymatrix,
       setmymatrixinv = setmymatrixinv,
       getmymatrixinv = getmymatrixinv)                          ## Returns the list of the functions setmymatrix, getmymatrix, 
  ## setmymatrixinv, and getmymatrixinv
  
}


## This function "cacheSolve" will return the inverse of the matrix created using "makeCacheMatrix" above.
## If the inverse had been previously computed, the value stored in the cache is returned and the redundant
## calculations are avoided.  If the inverse has not been computed, then the computation is completed using the 
## "solve" function, the results stored in the cache for future use, as well as returned by the function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  minv <- x$getmymatrixinv()                                     ## Retrieves the value that is in the cache for the matrix
  if(!is.null(minv)) {
    message("A cached value was found for this matrix, returning this value instead of re-computing it")
    return(minv)
  }                                                              ## Conditional check for the value stored for the inverse.
  ## If it was found to be NULL, then the inverse was not
  ## not computed.  If it is otherwise, the cache value is considered
  ## as a valid inverse of the matrix of interest, a message indicating
  ## that it will be used instead of re-computing, it is accordingly
  ## returned as the result, and the function exited without further 
  ## computation
  data <- x$getmymatrix()                                        ## This will retrieve the matrix from the cache
  minv <- solve(data, ...)                                       ## Compute the inverse
  x$setmymatrixinv(minv)                                         ## Store the inverse in the cache for subsequent uses
  minv                                                           ## Returns the inverese
  
}
