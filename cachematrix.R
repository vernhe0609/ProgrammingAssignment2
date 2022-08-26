## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##makeCacheMatrix consists of set,get,setinv,getinv

makeCacheMatrix <- function(x = matrix()) {
makeCacheMatrix <-function(x =matrix()){
  inv<-NULL                ##make inverse as NULL
  set <- function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x              ## function to get x
  setinv<-function(inversse)inv<<-inverse
  getinv<-function(){
    inver<-ginv(x)
    inver%%x                  ##function to get inverse of the matrix
  }
  list(set=set,get=get,
       setinv=setinv,
       getinv=getinv)
}
## Write a short comment describing this function
## This is used to get the cache data

cachesolve<-function(x,...)            ## get cache data
{inv<- x$getinv()
if(!is.null(inv)){                   ## check if invrse is NULL
  message("getting cached data!")
  return(inv)                      ## returns inverse value
}
data<-x$get()
inv<-solve(data,...)             ## calculates inverse value
x$setinv(inv)
inv                     ## retuns a matrix that is the inverse value of x
}

}

