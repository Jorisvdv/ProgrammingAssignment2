## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    #Initialise the inverse
    i <- NULL
    #Set matrix and set inverse to null
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    #Get matrix and return matrix
    get <- function() x
    
    #Set the inverse in the envoirment of the function
    setInverse <- function(inverse) i <<- inverse
    
    #Return the inverse
    getInverse <- function() i
    
    # Return a list of the methods of the function
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    #Get the inverse from the Cache function
    inv <- x$getInverse()
    #If inverse is set, return inverse
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    #Get matrix
    m <- x$get()
    
    # Calculate the inverse
    inv<-solve(m,...)
    
    #Set the inverse to the parent
    x$setInverse(inv)
    
    #Return the inverse
    inv
}
