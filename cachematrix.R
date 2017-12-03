## This function creates a special "matrix" object that can cache its inverse
## Assumed that the matrix supplied is always invertible.

## makeCacheMatrix makes a list containing a function to
##set the value of the vector
##get the value of the vector
##set the value of the mean
##get the value of the mean



makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getInverse <- function() m
        list(set = set, 
             get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
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
