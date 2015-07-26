## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix returns a matrix for use in cahceSolve to find the inverse
## and returns the inverse of the matrix stored in the cache
## cacheSolve checks for the inverse in the cache, if it is already computed, 
## It returns that matrix. If it is not, it computes the inverse and stores it
## in the cache. 
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    #Set initial value of inverse, none = NULL
    m <- NULL 
    #Create the matrix using the y variable in the global environment 
    set <- function(y){
        x <<- y    # Set matrix to be used to find the inverse
        m <<- NULL # set the value of inverse matrix to be Null. This will be 
        # be used by cacheSolve() to check if there is an inverse 
        # matrix already 
    }
    
    get <- function() x # Return cached matrix 
    setInverse <- function(solve) m <<- solve # Find the inverse of the cached
    # matrix and set it to the inverse
    # variable m
    getInverse <- function() m # return the inverse matrix
    list(set = set, get = get, # function list 
         setInverse = setInverse, 
         getInverse = getInverse)

}

#Check to see if there is an inverse matrix in the cache already 
cacheSolve <- function(x = matrix(), ...){
    m <- x$getInverse() # return the inverse stored in the cache
    if(!is.null(m)) { # check to see if there is an inverese already computed
        message("Getting cached data") # If yes, message returning there is an
        # inverse
        return(m) # return inverse matrix
    }
    
    #If null: 
    matrix <- x$get() #get matrix that will be used to find the inverse
    m <- solve(matrix, ...) #calculate the inverse of the matrix
    x$setInverse(m) #Set the inverse of the matrix to be stored in the cache
    m #Return matrix inverse
}

