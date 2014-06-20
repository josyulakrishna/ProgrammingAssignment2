## The makeCacheMatrix function accepts a matrix and creates a copy for itself please follow on to read more

makeCacheMatrix <- function(x = matrix()) {
	
## the inv variable is used to store the inverse of the passed matrix, its intially set to null
        inv <- NULL
        
## the set function is used to change the matrix stored in this function
## the inv is set to null to be recomputed by cacheSolve

        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
## the get function returns the matrix stored

        get <- function() x

## setinv function sets the inv varible to the inverse of the matrix

        setinv <- function(inverse) inv <<- inverse
        
## get inv function gets the inverse of the matrix which has been cached        

        getinv <- function() inv
        
## This returns the list of functions which can called from an external environment

        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


##The cacheSolve function sets the inverse of the matrix stored in the makeCacheMatrix function

cacheSolve <- function(x, ...) {

## get the inverse stored in the makeCacheMatrix and check if it exists if yes return the value
## if not compute the inverse of the matrix

        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        
## solve is used to compute the inverse of the matrix

        inv <- solve(data, ...)
        
## the inverse is set 

        x$setinv(inv)
        
## return the inverse

        inv
}