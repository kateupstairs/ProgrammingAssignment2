## The following functions are to cache the inverse of a matrix

## makeCacheMatrix creates a list to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse)inv<<-inverse
  getinverse<-function()inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The following functions returns the inverse of the matrix. 
## If the inverse has been computed, it returns the results and skip the computation. 
## If not, it sets the value in the cache through setinverse function.

cacheSolve <- function(x, ...) {
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached data.")
    return(inv)
  }
  data<-x$get()
  inv<-inverse(data)
  x$setinverse(inv)
  inv
}
