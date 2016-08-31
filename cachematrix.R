## Functions prepared as part of assignment for "R Programming" at coursera
## Author: Marcin Jankowski

## Function caches the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                            #inverse is null by default.   
    set <- function(nm) {                  #sets the initial matix
        inv<<- NULL                        #since matrix changed, inverse is set to null
        x <<- nm                           #matrix is set again
    }
    get <-function() {x}                   #function retutns the matrix
    setinv <- function(minv) {inv<<-minv}  #function sets the cached inverse
    getinv <- function() {inv}             #function gets cached inverse
    return(                                #returns a list of function names
        list(
            set=set, 
            get=get, 
            setinv=setinv,
            getinv=getinv
        )
    )
}


## Calculates the inverse. If the matrix didn't change and inverse is chached then 
## function returns cached copy. Otherwise function calculates the inverse and caches it

cacheSolve <- function(x, ...) {
    inv <- x$getinv()                      #get cached inverse
    if (is.null(inv)) {                    #if it is not cached
        inv<- solve(x$get(),...)           #calculate from x matrix. It gest matrix using x$get().
            x$setinv(inv)                  #set cached version to newly calculated
    }
    return(inv)   #returns inverse ()
}

