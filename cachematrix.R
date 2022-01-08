## Put comments here that give an overall description of what your
#functions do
#set a special matrix object
#get a special matrix object
#set the inverse of a matrix
#get the inverse of a matrix


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(invm) m<<- invm
  getinverse <- function() m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function:
# This function computes the inverse of the special matrix object
#returned by makeCacheMatrix above.

#If the inverse has already been calculated (and the matrix has not changed)
# then the cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
   m <-x$getinverse()  #return the inverse of x
   if (!is.null(m)){
     message("getting cached inverse of x")
     return(m)
   }
   data <- x$get()
   m<- solve(data,...)
   x$setinverse(m)
   m
   }


#mat <- makeCacheMatrix(matrix(1:6,2,3))
#mat$getinverse()
#mat$get()
#cacheSolve(mat)
