## MakecCacheMatrix function 
##  1. set function of the matrix : set initial x = y and m = NULL  
##  2. get function of the matrix : call x
##  3. set function of the inverse matrix : set m = inv  
##  4. get function of the inverse matrix .... can use 2 method
##     4.1 function() m : set inv = m or
##     4.2 function() solve(x) : set inv = solve(x)

makeCacheMatrix <- function(x = matrix()) {
             m <- NULL
             set <- function(y){
               x <<- y
               m <<- NULL
             }
             get <- function() x
             setinv <- function(inv) m <<- inv
             getinv <- function() m  # can use m with solve(x)
             list( set = set, 
                   get = get,
                   setinv = setinv,
                   getinv = getinv)
}

## CacheSolve function
## This function use to inverse matrix of x, use solve() 
## 1. get the inverse matrix 
## 2. if the inverse matrix don't make (use getinv <- function() m in makeCacheMatrix): get x matrix and make the inverse matrix by use solve(), then show the inverse matrix of x
## 3. if the inverse matrix already made (use getinv <- function() solve in makeCacheMatrix): show the inverse matrix.      
## Note : matrix x * inverse matrix x = I 
##        only square matrix can make the inverse matrix
##        some square matrix may not make the inverse matrix
##        we can ckeck these function use solve() with inverse matrix x ... should return matrix x 

cacheSolve <- function(x, ...) {
      m <- x$getinv()
      if(!is.null(m)) {
        message("getting cache data")
        return(m)
      }
      data <- x$get()
      m <- solve(data)
      x$getinv()
      m 
}       

## Check  
## solve(inverse matrix x) = matrix x
## > x
## [,1] [,2] [,3]
## [1,]    1    3    2
## [2,]    3    2    5
## [3,]    4    5    8

##  > inverseX <- cacheSolve(makeCacheMatrix(x))
##  > inverseX
## [,1] [,2]       [,3]
## [1,]  1.2857143    2 -1.5714286
## [2,]  0.5714286    0 -0.1428571
## [3,] -1.0000000   -1  1.0000000

## > y <- solve(inverseX)
## [,1] [,2] [,3]
## [1,]    1    3    2
## [2,]    3    2    5
## [3,]    4    5    8
## matrix y = matrix x
