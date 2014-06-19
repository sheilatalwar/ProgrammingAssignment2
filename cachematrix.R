## Matrix inversion is a costly computation. The functions below cache the inverse of a matrix 
## so that it won't have to be computed repeatedly.  

## makeCacheMatrix creates a special vector, which is a list containing functions that:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    s<-NULL
    set<- function(y) {
        x<<- y
        s<<- NULL
    }
    get <- function() x
    setInverse <- function(solved) s <<- solved
    getInverse <- function() s
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## cacheSolve computes the inverse of the matrix that can be retrieved from the vector above.  
## However, it first checks to see if the inverse has already been computed. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it computes the inverse of the matrix using the solve function.  

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s<-x$getInverse()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s<- solve(data)
    x$setInverse(s)
    s
}