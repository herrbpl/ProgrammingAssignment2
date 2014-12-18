## Put comments here that give an overall description of what your
## functions do
## This code is result of assignment in Proframming with R course in Coursera
## Programming Assignment 2: Lexical Scoping
## Original idea of how to use cachine is from: 
## https://github.com/rdpeng/ProgrammingAssignment2 by Roger D. Peng

## Functions help to cache results of previous results for given input. 

## This function wraps matrix and its inversion into kindda of object
## which uses lexical scoping to provide behaviour similar to objects in OOP 
## programming. 
## Function also checks if matrix is square-sized and if it has determinant=0

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
   
    # Function within function, sets input value and resets cached value
    set <- function(y) {
        # Assign to parent environment variable
        x <<- y
        m <<- NULL
    }
    # Function within function, returns argument
    get <- function() x
    # Function to set inversed matrix of original input
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    
    getInverseCalculate <- function(...) {
        
        if (!is.null(m)) {
            message("getting cached inverse")
            return(m)
        }
        
        if (ncol(x) != nrow(x) ) {
            
            warning("Number of Rows and columns of input are not equal")
            m <<- NA
            return(m)
            
        }
        
        if (det(x) == 0) {            
            warning("Matrix is singular and have no invertible matrix")
            m <<- NA
            return(m)
        }
        
        
        m <<- solve(x, ...)
        
        m
    }
    
    # set hooks
    list(set = set
        , get = get
        , setInverse = setInverse
        , getInverse = getInverse
        , getInverseCalculate = getInverseCalculate
         )
   
    
    
}


## This function takes input of type "makeCacheMatrix" and returns its 
## matrix inversed. This function is presented as is to comply with assignement 
## requirement "This function computes the inverse of the special "matrix" 
##  returned by makeCacheMatrix above. If the inverse has already been 
##  calculated (and the matrix has not changed), then the cachesolve 
##  should retrieve the inverse from the cache."

cacheSolve <- function(x, ...) {
    
    ref <- x
    
    if (is.array(x)) {
        
        t <- x        
        x <- makeCacheMatrix(t)
        ref <- x
        
    }
    
    m <- ref$getInverse()
    
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- ref$get()
        
    m <- solve(data, ...)
    ref$setInverse(m)
    m
    
    ## Return a matrix that is the inverse of 'x'
    
}



## This function takes input of type "makeCacheMatrix" and returns its 
## matrix inversed, but calculation is done in makeCacheMatrix

cacheSolve2 <- function(x, ...) {
    
    ref <- x
    
    if (is.array(x)) {
        
        t <- x        
        x <- makeCacheMatrix(t)
        ref <- x
        
    }
    
    m <- ref$getInverseCalculate()
        
    m
    
    ## Return a matrix that is the inverse of 'x'    
}


## Test Calculation:
## > rm(t)
## > t <- makeCacheMatrix(matrix(c(1,2,3,1,4,5,6,5,6), c(3,3)))
## > cacheSolve(t)
## [,1] [,2] [,3]
## [1,]  0.1 -2.4  1.9
## [2,] -0.3  1.2 -0.7
## [3,]  0.2  0.2 -0.2
## > cacheSolve(t)
## getting cached data
## [,1] [,2] [,3]
## [1,]  0.1 -2.4  1.9
## [2,] -0.3  1.2 -0.7
## [3,]  0.2  0.2 -0.2

## Test cache usage
##> t <- makeCacheMatrix(matrix(rnorm(1000*1000), c(1000,1000)))
## > system.time(cacheSolve(t))
## user  system elapsed 
## 1.48    0.00    1.48 
## > system.time(cacheSolve(t))
## getting cached data
## user  system elapsed 
## 0       0       0 
##> rm(t)


