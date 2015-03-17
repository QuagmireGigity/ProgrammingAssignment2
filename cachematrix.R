## makeCacheMatrix initaites a matrix and 
## creates a list of the functions(get, set for matrix and its inverse) that can be applied to it 

makeCacheMatrix <- function(x = matrix()) {
  
  x.inverse <- NULL
  set <- function(y) {
    x <<- y
    x.inverse <<- NULL 
  }
  get <- function() x
  setinverse <- function(i) x.inverse <<- i 
  getinverse <- function() x.inverse
  list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve function calculates inverse of a matrix and puts it in memory 
## so the every next call to the function can get it from memory rather than recumputing it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data<-x$get()
  i<-solve(data,...)
  x$setinverse(i)
  i
}
