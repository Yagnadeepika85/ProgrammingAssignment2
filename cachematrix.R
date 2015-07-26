## This piece of R code contains functions that cache the inverse of a matrix
##  Because matrix inversion is a computationally intensive process, especially when the size of the matrix is large,
## it is advisable to write a code that has the ability to cache the already-calculated inverse of a matrix, to avoid repeated computations

## makeCacheMatrix is a function that contains a list of functions namely get(),set(),getinverse() and setinverse().
## This is a function that creates a special matrix that helps cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y) {
    x<<-y
    i<<-NULL
  }
  get<-function() x 
  setinverse<-function(solve) i<<-solve
  getinverse<-function() i
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## cacheSolve() is a function that first checks whether a calculated matrix inverse is in memory, and if so, it returns
## the message "getting cached data" along with the value of the matrix inverse. 
##If not, the function calculates the inverse and returns the inverted matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<-x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  i<-solve(data, ...)
  x$setinverse(i)
  print(i)
}
