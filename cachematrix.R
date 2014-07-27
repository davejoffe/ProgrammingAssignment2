## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    n <- NULL
    set <- function(y){
        x <<- y
	n <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) n <<- inverse
    getinverse <- function() n
    list(set=set, get = get,
    setinverse = setinverse,
    getinverse = getinverse
}


## Write a short comment describing this function
## Computes the inverse using "solve". If the inverse has already been
## calculated the function retrieves the inverse from the cache
##
## Note - to run this function on a matrix x you need to do:
## cacheSolve(makeCacheMatrix(x))


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        n <- x$getinverse()
	if (!is.null(n)){
	    message("getting cached data")
	    return(n)
        }
	data <- x$get()
	n <- solve(data, ...)
	x$setinverse(n)
	n
}
