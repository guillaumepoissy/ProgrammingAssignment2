## These pair of functions allow to define and store a matrix as well as its inverse


## makeCacheMatrix creates a list containing functions that set the value of the
## matrix and its inverse (set / setinv) and get these matrices (get / getinv)

makeCacheMatrix <- function(x = matrix()) {
  inv<-matrix(,nrow(x),ncol(x))
  set <- function(y) {
    x <<- y
    
    inv <<- matrix(,nrow(x),ncol(x))
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
}



## cacheSolve returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        
  inv <- x$getinv()
  ## The content of inv is tested
  if(sum(is.na(inv))==0) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv
  
}
