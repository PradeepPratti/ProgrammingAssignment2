## The first function makeCacheMatrix takes a matrix as an argument and creates a special object
## that caches the inverse. The getmatrix function returns the matrix argument passed, the setmatrix
## function takes a matrix as an argument and assigns it to x. Getinverse returns the cached inverse
## value of matrix while setinverse computes and sets the inverse matrix that is passed to it.


makeCacheMatrix <- function(x = matrix()) 
{
      i <- NULL
      
      setmatrix <- function(y)
      {
            x <<- y
            i <<- NULL
      }
      
      getmatrix <- function() 
      {
            x
      }
      
      setinverse <- function(s) 
      {
            i <<- s
      }
      
      getinverse <- function() 
      {
            i
      }
      
      list(setmatrix = setmatrix,
           getmatrix = getmatrix,
           setinverse = setinverse,
           getinverse = getinverse)
}


## The following function takes the object created above and checks if the matrix inverse is already
## cached. If so, it returns the cached data. Else it computes and returns the inverse of the matrix

cacheSolve <- function(x) 
{
      ## Return a matrix that is the inverse of 'x'
      i <- x$getinverse()
      
      ## check if already cached
      if (!is.null(i))
      {
            message ("retrieving the cached data")
            return(i)
      }
      
      ## use getmatrix to retrieve the matrix passed to the previous function
      data <- x$getmatrix()
      
      ## compute inverse and set the value into cache.
      i <- solve(data)
      
      x$setinverse(i)
      i
}
