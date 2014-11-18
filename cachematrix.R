## Comments that give an overall description of what my
## functions do
## In these functions the inverse of a matrix will be determined, in the function makeCacheMatrix 
## the inverse matrix will be cached
## In the cacheSolve function the cached inverse function will be printed and when it not already consists 
## it will be determined again

## Short comment describing this function
## sets the matrix to global environment
## gets the matrix as in global environment
## determines and sets the inverse of the matrix using the solve function
## gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setinverse<-function(solve)m<<-solve
  getinverse<-function()m
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Short comment describing this function
## checks if inverse of the matrix is already determined
## if so it gets the inverse matrix from chache
## if not the inverse of the matrix is determined and sets the inverse matrix in the cache using setinverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data)
  x$setinverse(m)
  m     
}
