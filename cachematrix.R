## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        {
	 m<- NULL
	 set <- function(y)
		{
		 x <<- as.data.frame(y)
		 m <<- NULL
		}
	 get <- function() as.data.frame(x)
	 inverse <- function(x) { m <<- as.data.frame(solve(as.matrix(x)))}
	 getinverse <- function() as.data.frame(m)
	 list(set = set, get = get, inverse = inverse, getinverse = getinverse)
	}

}


## Write a short comment describing this function

cacheSolve <- function(x, ...)
       {
	 m <- x$getinverse()
	 if(!is.null(m))
	{
         
	 	  message("getting cached data")
        
	          return(m)
        
		}
        
	 data <- x$get()     
	 m <- inverse(data, ...)
        
	        
	 m
}
