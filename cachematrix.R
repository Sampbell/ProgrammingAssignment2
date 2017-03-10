## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function will cache a matrix inversion

makeCacheMatrix <- function(x=matrix()) { ## define default matrix
  inv <- NULL ##set inv as NULL
  set <- function(y) { ## define set function
    x<<-y ## assign value of matrix in parent environment
    inv<<-NULL
  }
  get<-function()x ## define get function
  setinverse<-function(inverse) inv <<- inverse ## Assign inv in parent environment
  getinverse<-function()inv ## gets value of inv
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

cacheSolve<-function(x,...) { ## find inverse of matrix x
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<x$get()
  inv<-solve(data,...)
  x$setinverse(inv)
  inv
}