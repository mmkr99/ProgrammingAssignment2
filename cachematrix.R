## makeCacheMatrix: This function creates a special "matrix" object that can cache
##its inverse.

##cacheSolve: This function computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve would retrieve the 
##inverse from the cache.


##The following function contains a list of functions to set a matrix 
##or get an already set matrix and to set inverse of a matrix or to get inverse 

makeCacheMatrix <- function(x = matrix()) {
  i = NULL
  set = function(m){
    x <<- m
    i <<- NULL
  }
  get = function(){x}
  setinverse = function(inverse){i <<- inverse}
  getinverse = function(){i}
  list(set = set,get = get,setinverse = setinverse,getinverse = getinverse)
}

##The following function helps to calculate the inverse of the matrix that is set 
##using the above function or if the inverse is already calculated, it can 
##restore it from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i = x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  else{
    matrix = x$get()
    i = solve(matrix)
    x$setinverse(i)
    i
  }
}
