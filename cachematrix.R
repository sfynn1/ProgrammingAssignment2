## Put comments here that give an overall description of what your
## functions do



## --------- TESTING FUNCTION -----------##
# generate a random matrix
testm <- matrix(sample.int(100, 5*5, TRUE), 5, 5)  #good
errM <- matrix( c(2,2,3,6,6,9,1,4,8) ,nrow = 3,ncol = 3) #explicit singular matrix

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

  invMat <- test$getInverse()
  if ( !is.null(invMat) ) {

        message("getting cached data")

    return(invMat)
  }
    else
         {
          originalMatrix <- test$get()

             # Test to ensure no errors, such as non-invertable matrix
              errorString <- NULL
              errorString <- tryCatch(solve(originalMatrix) %*% originalMatrix, error = function(e) e)  #capture any error
              if ( any(class(errorString) == "error") )   #test for error and exit
                     stop ("Singular Matrix - Please try a different matrix!")

              invMat <- solve(originalMatrix)
              test$setInverse(invMat)

          return(invMat)
         }
}

## --------------  References USED: ---------------- ##
#  https://cran.r-project.org/doc/manuals/R-intro.html#DOCF23  Open.Account example
#  http://stackoverflow.com/questions/24961983/how-to-check-if-a-matrix-has-an-inverse-in-the-r-language
