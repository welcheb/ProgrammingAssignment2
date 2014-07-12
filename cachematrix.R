## The pair of functions below work together to
# cache the inverse of a matrix

## Example usage:
"
# square matrix
a <- makeCacheMatrix( matrix(rnorm(9),3,3) ) 
a_inv <- cacheSolve(a)

# confirm cache ability
a_inv <- cacheSolve(a)

# confirm identity matrix
a_inv %*% a$get()

# non-square matrix
b <- makeCacheMatrix( matrix(rnorm(6),3,2) ) 
b_inv <- cacheSolve(b)

# confirm cache ability
b_inv <- cacheSolve(b)

# confirm identity matrix
b_inv %*% b$get()
"

## makeCacheMatrix creates a special "matrix", 
# which is really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the matrix_inverse
# 4. get the value of the matrix_inverse


makeCacheMatrix <- function(x = matrix()) {
  
  # make sure x is a matrix
  if ( !is.matrix(x) )
  {
    x <- matrix(x)
  }
  
  # initialize x_inverse
  x_inverse <- NULL
  
  set <- function(y) {
    x <<- matrix(y)
    x_inverse <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) x_inverse <<- inverse
  
  getinverse <- function() x_inverse
  
  list(set = set, get = get,
  setinverse = setinverse,
  getinverse = getinverse)
}


## cacheSolve returns a matrix that is the inverse of 'x'
# if the inverse of x has already been solved, the function
# returns the cached inverse

cacheSolve <- function(x, ...) {
  
  # Return a matrix that is the inverse of 'x'
  x_inverse <- x$getinverse()
  
  # check if inverse has already been calculated
  if(!is.null(x_inverse)) {
    message("getting cached inverse")
    return(x_inverse)
  }
  
  # otherwise need to calculate inverse matrix
  data <- x$get()
  
  # solve works only for square matrices
  #x_inverse <- solve(data, ...)
  
  # ginv (see below) works for square or non-square matrices
  x_inverse <- ginv(data)
  x$setinverse(x_inverse)
  x_inverse
}

## Generalized Inverse of a Matrix
#
# Taken from the example shown when typing ??ginv 
#
ginv <- function(X, tol = sqrt(.Machine$double.eps))
{
  
  dnx <- dimnames(X)
  if(is.null(dnx)) dnx <- vector("list", 2)
  
  # harness the singular value decomposition of a matrix
  s <- svd(X)
  
  nz <- s$d > tol * s$d[1]
  structure(
    if(any(nz)) s$v[, nz] %*% (t(s$u[, nz])/s$d[nz]) else X,
    dimnames = dnx[2:1])
}
