## This file contains code for two functions (makeCacheMatrix and cache Solve)
## makeCacheMatrix is a function which give list as output which contains 4 functions set,
## get, setinv, getinv. 

## cacheSolve matrix checks if the inverse of a matrix is already calculated
## and if already calculated, its retrieves the inverse from cache i.e using one of the ## ## ## function defined in makeCacheMatrix

## Assumption:input matirx is alwasy non-singular

## Below function retrieves the list as output with 4 functions

makeCacheMatrix <- function(x = matrix()) {


inv<-NULL
set<-function(y)
{
   x<<-y
   inv<<-NULL
}

get<-function() x

setinv<-function(x) 
{
inv<<-solve(x)
}
getinv<-function() inv

list (set=set, get=get, setinv=setinv, getinv=getinv)


}


## Below  function is to find the inverse of a matrix , if inverse is already cached then it retrieves ## from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

inv<-x$getinv()

if(!is.null(inv))
{
message("getting cached data")
return(inv)

}

data<-x$get()
inv<-solve(data,...)
x$setinv(inv)
inv


}
