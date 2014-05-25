##makeCacheMatrix is a special object which contains the getter and setter for the matrix and its inverse.
##Until the setter function is not called the iMatrix variable will have a NULL value
##when cachesolve function is called then the inverse of the matrix is assigned to iMatrix variable
##when cachesolve is called the second time then the iMatrix variable is returned which is in
##lexical scoping

makeCacheMatrix <- function(x = matrix()) {
  iMatrix <- NULL
  set <- function(y) {
    x <<- y  ##caching the orginal matrix
    iMatrix <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(inverseMatrix) iMatrix <<- inverseMatrix #caching the inverse of matrix
  getInverseMatrix <- function() iMatrix
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## cachesolve will take a special object created by makeCacheMatrix function.
## if cachesolve is called first time then inverse is calculated and cached
## next time the cachesolve is called on the same object, cached inverse will be returned

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
  iMatrix <- x$getInverseMatrix()
  # Check whether inverse is calculated
  if(!is.null(iMatrix)) {
    message("getting cached inverse of the matrix")
    return(iMatrix)
  }
  # if the inverse of matrix is not yet calculated then iMatrix will be NULL
  # and we will calculate the inverse and return the inverse of the matrix.
  data <- x$get()
  if(det(data) > 0){ #solve() will throw error if determinant is less than or equal to zero
    iMatrix <- solve(data)
    x$setInverseMatrix(iMatrix)
    return (iMatrix)
  }else{
    message("Matrix is not invertible")
  }
  
}