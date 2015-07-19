## Functions for cache the inverse of a matrix
##
##> m<-makeCacheMatrix()
## Create a special "matrix", which is a list containing
##> m$set(matrix(c(1:4), c(2, 2)))
##> cacheSolve(m)
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
  set <- function(y){
  x<<-y
  m<<-NULL
}
get<-function() x
setmatrix<-function(solve) m<<- solve
getmatrix<-function() m
list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {cacheSolve <- function(x=matrix(), ...) {
    m<-x$getmatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}
