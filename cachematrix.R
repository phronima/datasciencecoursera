## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Creates a special "matrix" object (actually a list
## of functions that set and return the matrix and 
## its inverse (when calculated)).

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        ## reserves the variable 'inv' for the inverse
                
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## sets 'x' in the parent environment (so
        ## it can be accessed by the other functions)
        ## to the argument of ...$set() and similarly
        ## resets the variable 'inv' for the inverse
        
        get <- function() x
        ## returns the matrix stored in 'x'
        
        setinv <- function(solved) inv <<- solved
        ## updates 'inv' in the parent environment
        ## with the inverse when calculated
        
        getinv <- function() inv
        ## returns the inverse
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        ## assigns the names in the list
        ## to be the names of the funtions defined

}


## Write a short comment describing this function
## Calculates the inverse of the matrix created by 
## makeCacheMatrix(), or retrieves it if it has been cached
## Assumes the matrix is invertible
## (i.e., det(x)!=0 & dim(x)[1]==dim(x)[2])

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Note, strictly 'x' is the list made with makeCacheMatrix(),
        ## not the matrix itself.
        
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## Check if inverse has already been cached --
        ## if it has, return it.
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        ## Otherwise, get the matrix from 'x',
        ## invert, and set cache 
        
        inv
        ## return the inverse
}
