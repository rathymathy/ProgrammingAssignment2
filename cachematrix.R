## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##There are 2 functions makeCacheMatrix,makeCacheMatrix
##makeCacheMatrix consist of set,get,setinv, getinv
##library(MASS) is used to calculate inverse for non squared and square matrices
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL            #initializing inverse as NULL
  set<-function(y){
                  x<<-y
                  inv<<-NULL
                   }
  get<-function()x             # to get matrix x
  setinv<-function(inverse)inv<<-inverse
  getinv<-function(){ 
                    inver<-ginv(x)
                    inver%*%x           #to obtain inverse of the matrix
                    }
  list(set = set, get = get, 
       setinv = setinv, 
       getinv = getinv)
}


## Write a short comment describing this function
##below one is to get the cache data



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         inv<-x$getinv()                  
  if(!is.null(inv)){                 #checking inverse is NUll 
                     message("getting cached data!")
                     return(inv)                       #return inverse value
  }
  data<-x$get()
  inv<-solve(data,...)              #calculate inverse value
  x$setinv(inv)
  inv
}
