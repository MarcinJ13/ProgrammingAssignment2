## Put comments here that give an overall description of what your
## functions do

## Function caches the matrix and its inverse
## Functions prepared as part of assignment for "R Programming" at coursera
## Author: Marcin Jankowski

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(nm) {
      inv<<- NULL
      x <<- nm
  }
  get <-function() {x}
  setinv <- function(minv) {inv<<-minv}
  getinv <- function() {inv}
  return(
        list( set=set, 
        get=get, 
        setinv=setinv,
        getinv=getinv
    )
  )
}


## Calculates the inverse. If the matrix didn't change and inverse is chached then it returns chached copy
## otherwise function calculates it

cacheSolve <- function(x, ...) {
        inv <- x$getinv() #get cached inverse
        if (is.null(inv)) { #if it is not cached
            inv<- solve(x$get(),...)  #calculate from x matrix
            x$setinv(inv)  #set cached version to newly calculated
        }
        return(inv)   #returns inverse ()
}









