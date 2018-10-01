##  Assignment 2.  Use lexical scopiong to determine how R finds elements to get values while executing.  


##  makeCacheMatrix function: Create a special "matrix" object that can cache its inverse 
##  Following code and logic of MakeVector function example, but inverse instead of mean

makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set<-function(y) {
    x<<-y
    inv<<-NULL
  }
  get <-function()x
  setinverse<-function(inverse) inv<<-inverse  
  getinverse<-function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


##  cacheSolve function:  Compute the inverse of the special "matrix" returned  by makeCacheMatrix.  
##  If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
##  Following code and logic of cacheMean function in example, but for inverse instead of mean

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<-x$getinv()
  if(!is.null(inv)) {
    message("this is cached result")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinv(inv)
  inv   
}
