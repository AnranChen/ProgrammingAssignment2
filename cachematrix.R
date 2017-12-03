## Assumed that the matrix supplied is always invertible.



## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, 
         get=get, 
         setinverse=setinverse, 
         getinverse=getinverse)
}


## cacheSolve returns a matrix that is the inverse of x

 cacheSolve <- function(x, ...) {
     inv <- x$getinverse()
     if(!is.null(inv)) {
         message("getting cached data.")
         return(inv)
     }
     data <- x$get()
     inv <- solve(data)
     x$setinverse(inv)
     inv
}

##Sample Run
##> m <- matrix(1:4,2)
##> m1 <- makeCacheMatrix(m)
##> m1$get()
##     [,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> cacheSolve(m1)
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> cacheSolve(m1)
##getting cached data.
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
> 
