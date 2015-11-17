## The following fuctions are for R Programming Assignment #2: Lexical Scoping
#
#  This file contains all necessary functions, testing code and references to
#  complete the assignment.
#  Purpose of the assignment was to become familiar with the benefits and
#  nuances of lexical scoping within the R language.  The example used was
#  the ability to execute a computation and cache it for use in other functions.
#  This demonstrates the ability to reference objects globally and reduce
#  unnecessary re-computations.
#

## --------------  References ---------------- ##
#  Major references used eyond the course materials:
#
#  https://cran.r-project.org/doc/manuals/R-intro.html#DOCF23  Open.Account example
#  http://stackoverflow.com/questions/24961983/how-to-check-if-a-matrix-has-an-inverse-in-the-r-language
## ------------------------------------------- ##

## --------- BEGIN:  TEST FUNCTION ---------- ##
## The following can be uncommented and executed to test the functions
#
# testm <- matrix(sample.int(100, 5*5, TRUE), 5, 5)     # generate a random matrix
# errM <- matrix( c(2,2,3,6,6,9,1,4,8) ,nrow = 3,ncol = 3)    # explicit singular matrix
# test <- makeCacheMatrix(testm)
# errM <- makeCacheMatrix(testm)
# test$get()
# errM$get()
# test$getInverse()
# errM$getInverse()
# cacheSolve(test)
# cacheSolve(test)
# test$getInverse()
# cacheSolve(errM)
#
## --------- END:  TEST FUNCTION ---------- ##

## -------------- BEGIN - FUNCTIONS --------------- ##
#  makeCacheMatrix()  -  Creates a new object that creates
#                        and caches the inverse of a given matrix

makeCacheMatrix <- function(original = matrix()) {

      inverseMatrix <- NULL

      set <- function(original)
        {
         originalMatrix <<- original
         inverseMatrix <<- NULL
        }

      get <- function() original

      setInverse <- function(original) inverseMatrix <<- original

      getInverse <- function() inverseMatrix

      #output
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)

}
#  cacheSolve()  -  Computes the inverse of a give matrix returned by makeCacheMatrix.
#                   If the inverse has already been calculated for a given matrix,
#                   then retrieve the inverse from the cache.  Do not recompute!
#
cacheSolve <- function(origMat, ...) {

    invMat <- test$getInverse()

        if ( !is.null(invMat) )
            {
                 message("getting cached data")

              return(invMat)
             }
        else
             {
              originalMatrix <- test$get()
                 # OPTIONAL FEATURE ONLY - Learning R #
                  errorString <- NULL

                  # Test to ensure no errors, such as non-invertable matrix
                  errorString <- tryCatch(solve(originalMatrix) %*% originalMatrix, error = function(e) e)  #capture any error
                  if ( any(class(errorString) == "error") )   #test for error and exit if exists
                         stop ("Possible singular matrix error - Please try a different matrix!")

                  invMat <- solve(originalMatrix)
                  test$setInverse(invMat)

              return(invMat)
             }
}
## -------------- END - FUNCTIONS --------------- ##

