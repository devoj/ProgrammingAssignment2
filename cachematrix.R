## This function will solve the inverse of a Matrix using "solve()"
## To fulfill the requirements of the assingment, the program will cache the 
## inverse matrix after computing. The program will also recall the inverse
## from cache if it has already been computed. 

## This function will store the matrix object, create a list of functions and 
## solve for the matrix inverse.
makeCacheMatrix <- function(x = matrix()) {
        # Create empty variable inv
        inv <- NULL
        
        # Function to set new matrix
        set <- function(new_matrix) {
                x <<- new_matrix
                inv <<- NULL
        }
        
        # Function to retrieve the cacehed matrix
        get_matrix <- function() x
        
        # Function to solve/store inverse matrix, use 
        compute_inv <- function(solve) inv <<- solve
        
        # Function to retrieve the cached inverse matrix
        get_inv <- function() inv
        
        # Set names to list of functions for retrieval
        list(set = set, get_matrix = get_matrix,
             compute_inv = compute_inv,
             get_inv = get_inv)
        
}


## This function will check to see if the inverse matrix "inv" has already been
## computed or is NULL. If not NULL, which means it as been already computed, it
## will then try to retrieve it from the cache. If it is Null, then it will 
## compute the inverse and store it in cache.

cacheSolve <- function(x, ...) {
        
        #First, get the value for the inverse matrix from the list created in 
        # makeCachMatrix
        inv <- x$get_inv()
        
        #Check to see if not null, if true, then it will retreive the cached
        #Value
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        #If not null was false, it will retrieve the matrix from makeCacheMatrix
        matrix <- x$get_matrix()
        
        #Then compute inverse.
        inv <- solve(matrix, ...)
        
        #Finally, it will store it.
        x$compute_inv(inv)
        
        #Call the inverse matrix variable to display the inverse matrix
        inv
}