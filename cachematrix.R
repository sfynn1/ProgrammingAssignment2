## Put comments here that give an overall description of what your
## functions do



## --------- TESTING FUNCTION -----------##
# generate a random matrix
testm <- matrix(sample.int(100, 5*5, TRUE), 5, 5)


## Write a short comment describing this function

makeCacheMatrix <- function(original = matrix()) {
# Take matrix arguemnt and load to global memory

  inverseMatrix <- NULL
  set <- function(original) {
    originalMatrix <<- original
    inverseMatrix <<- NULL
  }

  get <- function() original

  setInverse <- function(original) inverseMatrix <<- original

  getInverse <- function() inverseMatrix

  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(origMat, ...) {
        ## Return a matrix that is the inverse of 'x'

#   m <- x$getmean()
#   if(!is.null(m)) {
#     message("getting cached data")
#     return(m)
#   }
#   data <- x$get()
#   m <- mean(data, ...)
#   x$setmean(m)
#   m

  invMat <- test$getInverse()
  if (!is.null(invMat)) {
    message("getting cached data")
    return(invMat)
  } else {
  originalMatrix <- test$get()
  invMat <- solve(originalMatrix)
  test$setInverse(invMat)

  return(invMat)
  }
}


## References USED:
#  https://cran.r-project.org/doc/manuals/R-intro.html#DOCF23  Open.Account example
