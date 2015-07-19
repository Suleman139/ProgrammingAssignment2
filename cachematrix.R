## Below are two functions that are used to create a special object that stores a matrix and 
## cache's its inverse.

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to:
## 1)set the value of the matrix (set)
## 2)get the value of the matrix (get)
## 3)set the value of the inverse (set_inverse)
## 4)get the value of the inverse (get_inverse)

##> m<-makeCacheMatrix()
## Create a special "matrix", which is a list containing
##> m$set(matrix(c(1:4), c(2, 2)))
##> cacheSolve(m)
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL                                 ## Initially assigning 'NULL' to inverse
  set <- function(y){
  x<<-y						## Setting the matrix 'x'	
  m<<-NULL
}
get<-function() x					## Returning matrix 'x'
setmatrix<-function(solve) m<<- solve     ## Cache the value of the inverse 
getmatrix<-function() m				## Returning inverse
list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}



## The following function calculates the inverse of the special "matrix" created with the above function.
## However, it first checks to see if the inverse(m) has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via 
## the 'setmatrix' function.


cacheSolve <- function(x, ...) {cacheSolve <- function(x=matrix(), ...) {   ## Return a matrix that is the inverse of 'x'
    m<-x$getmatrix()				## Getting inverse
    if(!is.null(m)){				## Checking for the presence of inverse
      message("getting cached data")	## Displaying message
      return(m)
    }
    matrix<-x$get()				## Getting Matrix
    m<-solve(matrix, ...)			## Using solve() to compute inverse
    x$setmatrix(m)				## To cache the inverse
    m							## Returning the inverse
}
