## PROGRAMMING ASSIGNMENT 2: LEXICAL SCOPING 

## TASK: Write a pair of functions that cache the inverse of a matrix

# Function 1 - makeCacheMatrix(): This function creates a special "matrix" object
# that can cache its inverse

# This function creates an environment having 4 functions and 2 objects 
# 2 objects include: 
#         x: object stores input matrix to find inverse matrix
#         m: object stores inverse matrix after calculation

# # 4 functions including: 
# 1. get function, which will retrieve matrix x
# 2. set function, which will get the value of y to x and reset the value 
# of m - the stored inverse matrix back to NULL in case of recalculation
# 3. setInverse function, which will set the value of inverse matrix 
# calculated in cacheSolve (Function 2) to m 
# 4. getInverse function, which will retrieve matrix m


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL 
        set <- function(y) {
                x <<- y
                m <- NULL 
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m 
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}


# # Function 2 - cacheSolve(): does 2 things, check whether the inverse matrix was calculated,
# if yes: it will retrieve the value of the inverse matrix
# if no: it will calculate the value of inverse matrix and store it in environment
# created by makeCacheMatrix 


cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if (!is.null(m)) {
                message("return from cache")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}