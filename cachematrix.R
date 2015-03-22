## The following functions allow to compute a matrix inverse while storing 
## the matrix data and some ancillary functions in a list

## The function makeCacheMatrix provides a set of functions 
## (set, setinverse, get, getinverse) and stores the matrix, 
## giving as output a list

makeCacheMatrix <- function(x = matrix()) { 
#sets the variable inv to NULL
        inv <- NULL
#defines the function set
        set <- function(y) {
#x (resp., inv) is super-assigned to be equal to y (resp., NULL)
                x <<- y                                               
                inv <<- NULL                            
        }                                               
        get <- function() x
# the function setinverse() sets the value of inv
        setinverse <- function(inverse) inv <<- inverse
# the function getinverse() returns the value of inv
        getinverse <- function() inv  
#create a list that allows to retrieve functions and data
        list(set = set, get = get,                      
             setinverse = setinverse,                  
             #
             getinverse = getinverse)                   
}


## The function takes as input the list produced by makeCacheMatrix, 
## checks if the inverse matrix is stored in the cache and displays it


cacheSolve <- function(x, ...) {
#assign to the variable inv the value of the output of getinverse
        inv <- x$getinverse()                   
#If inv is not null, the variable inv is retrieved from cache
        if(!is.null(inv)) {                     
                message("getting cached data")  
                return(inv)                     
        }                                       
        data <- x$get()                         
#the matrix is inverted using solve()
        inv <- solve(data, ...)
#inv is superassigned with the value obtained from computation and then displayed
        x$setinverse(inv)                      
        inv                                     
}
