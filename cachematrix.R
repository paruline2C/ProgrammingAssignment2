## R Programming - Week 3 - Programming Assignment 2: Lexical Scoping 
## Theses functions allows to cache the Matrix inverse instead of computing each time.
## 
## Use :
## 1) MakeCacheMatrix to create the "matrix"  st#ructure
## 2) Call of cacheSolve using the structure

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL;
  
  #set the new matrix value
  setMat <- function(y) {
     x <<- y;
     inv <<- NULL;
  }
  
  # get the matrix values
  getMat <- function() x;
  
  ## set new inverse
  setinv <- function(new_inv)  inv <- new_inv;
  
  ##get the computed inverse
  getinv <- function() inv;
  
  ##output : list of functions
  return (list (
         getMat = getMat,
         setMat = setMat,
         setinv = setinv,
         getinv = getinv) )
  
}


## Return a matrix that is the inverse of 'x' with X list created by "makeCacheMatrix"
cacheSolve <- function( x, ...) {
   
  ## Try to recover the inverse
  inv_x <- x$getinv();
  
  
  ## Inverse not existing in cache
  if (!is.null(inv_x) )
  { 
    print("Inverse existing in cache")
    return(inv_x)
 
  }
  else
  {
    print("No Inverse existing")
    Mat_value <-x$getMat()
    new_inv <- solve(Mat_value)
    x$setinv(new_inv)
    return(x$getinv())
  }
  
}
