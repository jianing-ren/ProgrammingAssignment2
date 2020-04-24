## Put comments here that give an overall description of what your
## functions do
## getting the inverse of the matrix

## Write a short comment describing this function


makeCacheMatrix = function(x = matrix()) {
    m <- null
    set=function(y)
    {
        x <<- y
        m <<- NULL
        get <- function()
        {x}
        set_inv=function(inverse)
        {
            m <<- inverse 
        }
        get_inv=function() m
        list(set=set, get=get, set_inv=set_inv, get_inv=get_inv)
    }

}




## Write a short comment describing this function
## calculate the inverse of the matrix but check first if it is calculated. 

cacheSolve=function (x,...)
{
    m<-x$get_inv()
    if (!is.null(m))
    {
        message("getting cached data...")
        return (m)
    }
    data<-x$get()
    m<-inverse(data,...)
    x$set_inv(m)
    m
}
