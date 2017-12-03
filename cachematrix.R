## This function creates a special "matrix" object that can cache its inverse

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
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, 
             get = get, 
             setmean = setmean,
             getmean = getmean)
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
