## Computation of inverse of a matrix by using caching
## If the contents of a matrix have not changed, it
## caches the inverse of the matrix so that it can be looked up in the cache 
## Note: Program assumes that the supplied matrix is an invertible matrix.
##
## makeCacheMatrix function creates a special "matrix" object that can cache its inverse. 
## It creates a list containing a function to set the value of the matrix, get the value
## of the matrix, set the inverse of the matrix, get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL  ## Initializes the inverse of the matrix to NULL
        
     ## Function to initialize both the matrix and inverse of matrix to NULL
        set <- function(y) {
                x <<- y     ## Initializes the matrix to y
                m <<- NULL  ## Initializes the inverse of the matrix to NULL
        }
        
        get <- function() x   ## Function to get the original matrix   
        setinv <- function(solve) m <<- solve  ## Function to set the inverse of the matrix
        getinv <- function() m   ## Function to get the inverse of the matrix  
        
     ## Creates a List with all the functions and name them accordingly so that it is convenient to call    
        list(set = set, get = get,  
             setinv = setinv,
             getinv = getinv)

}


##  cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
##  should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {

        m <- x$getinv()  ## Call to getinv() function which returns the inverse of matrix
      
     ## Function to check whether cached inverted matrix exists
        if(!is.null(m)) {
                message("getting cached Inverted Matrix")  ## Print that inverted matrix exists in cache
                return(m)  ## If cached inverted matrix exists, return the Cached matrix without computing it again
        }
        
        matrix <- x$get()  ## Call to get() function which returns the original matrix
        m <- solve(matrix, ...)  ## Compute the inverse of the matrix 
        x$setinv(m)  ## Call to setinv() function which sets the inverse of the matrix as computed above
        
        m  ## Return a matrix that is the inverse of 'x'
}
