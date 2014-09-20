## Put comments here that give an overall description of what your
## functions do

## Function makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y) {
      x <<- y
      inv <<- NULL
   }
   get <- function() x
   setSolve <- function(solve) inv <<- solve(x)
   getSolve <- function() inv
   list(set = set, get = get,
      setSolve = setSolve, getSolve = getSolve)
}

## Function cacheSolve
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## Computing the inverse of a square matrix can be done with the solve function in R.
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.
## For this assignment, it is assumed that the matrix supplied is always invertible.
cacheSolve <- function(x, ...) {    
   inv <- x$getSolve()
   if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
   }
   data <- x$get()
   inv <- solve(data, ...)
   x$setSolve(inv)
   inv
}

# Test function 
# Usage: test() - Will run the test cases underneeth. 
# This consist only out of a simple test case. 
test <-function() {
	message("Create test function for makeCacheMatrix and cacheSolve")
	A = matrix(c(3, 0, 2,  
	             2, 0, -2,
				 0, 1, 1), 
				 nrow=3, ncol=3)
    print("inverse matrix A")
	print(A)
	message("check if matrix A an be solved, it can :)")
    print(solve(A))
	print("matrix <-makeCacheMatrix(A)")
	matrixA <-makeCacheMatrix(A)
    print("Before Caching------------------------------------------") 
	print("matrixA$get():")
	print(matrixA$get())
    print("matrixA$getSolve()-Should be Null since it is not yet solved:")	
	print(matrixA$getSolve())
	inv_result<-cacheSolve(matrixA)
	print("manually solving it with matrixA$setSolve()");
	matrixA$setSolve(matrixA$get())
	print("matrixA$getSolve()-It should show the Inverse Matrix:")	
	print(matrixA$getSolve())
	print("After Caching-It should show the inverse Matrix:")
	print(inv_result)
	print("After re - caching-It should show the inverse Matrix:")
	inv_result2<-cacheSolve(matrixA)
	print(inv_result2)
}

####TestOutput 20 Dec Run on Windows 7
####> test()
####Create test function for makeCacheMatrix and cacheSolve
####[1] "inverse matrix A"
####     [,1] [,2] [,3]
####[1,]    3    2    0
####[2,]    0    0    1
####[3,]    2   -2    1
####check if matrix A an be solved, it can :)
####     [,1] [,2] [,3]
####[1,]  0.2 -0.2  0.2
####[2,]  0.2  0.3 -0.3
####[3,]  0.0  1.0  0.0
####[1] "matrix <-makeCacheMatrix(A)"
####[1] "Before Caching------------------------------------------"
####[1] "matrixA$get():"
####     [,1] [,2] [,3]
####[1,]    3    2    0
####[2,]    0    0    1
####[3,]    2   -2    1
####[1] "matrixA$getSolve()-Should be Null since it is not yet solved:"
####NULL
####[1] "manually solving it with matrixA$setSolve()"
####[1] "matrixA$getSolve()-It should show the Inverse Matrix:"
####     [,1] [,2] [,3]
####[1,]  0.2 -0.2  0.2
####[2,]  0.2  0.3 -0.3
####[3,]  0.0  1.0  0.0
####[1] "After Caching-It should show the inverse Matrix:"
####     [,1] [,2] [,3]
####[1,]  0.2 -0.2  0.2
####[2,]  0.2  0.3 -0.3
####[3,]  0.0  1.0  0.0
####[1] "After re - caching-It should show the inverse Matrix:"
####getting cached data
####     [,1] [,2] [,3]
####[1,]  0.2 -0.2  0.2
####[2,]  0.2  0.3 -0.3
####[3,]  0.0  1.0  0.0

