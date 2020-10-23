## Put comments here that give an overall description of what your
## functions do: creates a special "matrix" object that can cache its inverse
## and the second function checks whether the inverse has been calculated

## Write a short comment describing this function
## creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    invers <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    set_invers <- function(inverse) invers <<- inverse
    get_invers <- function() invers
    list(
        set = set,
        get = get,
        set_invers = set_invers,
        get_invers = get_invers
    )
}


## Write a short comment describing this function
## check whether the inverse has been calculated
## if yes, the matrix is not changed, and the inverse is retrieved from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invers <- x$get_invers()
    if(!is.null(invers)){
        message("getting cached data")
        return(invers)
    }
    data <- x$get()
    invers <- solve(data, ...)
    x$set_invers(invers)
    invers
}
