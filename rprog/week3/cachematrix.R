### ASSIGNMENT ### 

## This function instantiates an special Matrix object
## by taking in a matrix and providing it with additional 
## functionality to allow for caching 

makeCacheMatrix <- function(x = matrix()) {  
  # inverse is not yet solved upon creation
  i <- NULL   
  # helper function to return the matrix
  get <- function() {
    x
  }
  # function to store the inverse
  set_inverse <- function(inverse) {
    i <<- inverse
  }
  # function to return the inverse
  get_inverse <- function() {
    i
  }

  list(get = get, set_inverse = set_inverse, 
       get_inverse = get_inverse)
}


## This function takes a 'CacheMatrix' object created by
## the above function, makeCacheMatrix

cacheSolve <- function(x, ...) {
  # tries to pull cached solution
  i <- x$get_inverse()
  
  if(!is.null(i)) {
    message("getting cached solution")
    return(i)
  }
  
  # if solution is not cached, it pulls the matrix
  # solves for the inverse and sets that value to the CacheMatrix
  # and returns the inverse
  matrix <- x$get()
  i <- solve(matrix, ...)
  x$set_inverse(i)
  i
}

### TEST ###
m <- matrix(c(-1, -2, 1, 1), 2,2)
x <- makeCacheMatrix(m)
x$get()
#       [,1] [,2]
# [1,]   -1    1
# [2,]   -2    1

inv <- cacheSolve(x)
inv
#       [,1] [,2]
# [1,]    1   -1
# [2,]    2   -1

inv <- cacheSolve(x)
# getting cached data
inv
#        [,1] [,2]
# [1,]    1   -1
# [2,]    2   -1