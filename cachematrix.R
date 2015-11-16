## Put comments here that give an overall description of what your
## functions do



## --------- TESTING FUNCTION -----------##
# generate a random matrix
testm <- matrix(sample.int(100, 5*5, TRUE), 5, 5)


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
# Take matrix arguemnt and load to global memory
  m1 <<- x
  m1inverse <<- solve(x)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  if(x==m1){
     output <- m1inverse
  }  else
  { output <- solve(x)}
  return(output)
}
