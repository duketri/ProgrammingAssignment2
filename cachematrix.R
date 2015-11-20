## Put comments here that give an overall description of what your
## functions do

## initially sets the inverse i to NULL
## the set function sets up the initial matrix assignment function which sets x as y and i as Null,  
## the get command returns the x matrix
## the setinverse command finds the inverse of the matrix,
## the getinverse command returns the inverse, 
## the list command then returns set, get, setinverse, and getinverse within the special matrix


makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## initially sets i by calling the getinverse function for matrix x 
## if i is Null and the inverse has been calculated already, returns the cached inverted matrix
## if i is not Null, sets data by calling the get function for matrix x
## sets i as the inverse of data
## then sets the inverse in the cache and returns the inverted matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
  
}
