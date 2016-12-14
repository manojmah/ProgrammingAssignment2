#we have to write the function for a inverse of a matrix 
#lets first write the function to cache the inverse of a matrix
#below function creates an object that stores a matrix and caches its inverse
makeCacheMatrix <- function(x = matrix()){
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
##this function computes the inverse of the matrix created by above function
##it checks if the inverse alreadyy in the cache then it will return the cached value
##if cache not found then it will compute the inverse and save it in the cache 

cacheSolve <- function(x, ...){
  #return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  mat_data <- x$get()
  i <- solve(mat_data, ...)
  x$setinverse(i)
  i
}	
