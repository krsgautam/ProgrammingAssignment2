## The functions below make use of R's scoping rules to create an object 
## in an environment that is different from the current environment by 
## using the <<- operator to catch/store the results of the #potentially 
## time consuming matrix inversion, which can be retrieved later instead 
## of redoing the #computing.  This involves two functions as described below. 

## The first function, makeCacheMatrix, creates a list containing a function to: 
##  (i) set the object of the #matrix, 
##  (ii) get the object of the matrix, 
##  (iii) set the object of the inverted matrix, and 
##  (iv) get the #object of the inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
      z <- NULL
      set <- function(y) {
          x <<- y
          z <<- NULL
    }
      get <- function() x
      setsolve <- function(solve) z <<- solve
      getsolve <- function() z
      list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## The second function, cacheSolve, calculates the inverse of the matrix 
## created with the above #function. However, it first checks to see if the 
## inverse has already been calculated. If so, it gets the #inverse from the 
## cache and skips the computation. Otherwise, it derives the inverse of the 
## matrix and sets the value of inverted matrix in the cache via the setsolve 
## function.

cacheSolve <- function(x, ...) {
     z <- x$getsolve()
     if(!is.null(z)) {
        message("getting cached data")
        return(z)
      }
      data <- x$get()
      z <- solve(data)
      x$setsolve(z)
      z
}
mymatrix <- makeCacheMatrix(matrix(rnorm(9), nrow=3,ncol=3))
cacheSolve(mymatrix)


