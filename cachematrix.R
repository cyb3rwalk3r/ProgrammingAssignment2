#function to create an empty matrix
makeCacheMatrix <- function(mat = matrix())
{
        matInverse <- NULL
        #function to set the value of matrix
        set <- function(m)
        {
                mat <<- m
                matInverse <<-NULL
        }
        
        #function to get the value of matrix
        get <- function()
        {
                mat
        }
        
        #function to set the value of inverse matrix
        setInverse <- function(inv)
        {
                matInverse <<- inv
        }
        
        #function to get the value of inverse matrix
        getInverse <- function()
        {
                matInverse
        }
        
        list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}

#function to store the inverse of a matrix in cache
cacheSolve <- function(x, ...) 
{
        m <- x$getInverse()
        
        #checking if inverse matrix has already been computed
        if(!is.null(m)) 
        {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInverse(m)
        m
}