# For Programming Assignment 2 of "R Programming"

# This function creates a special "matrix" object that can caches its inverse 
makeCacheMatrix <- function(x=matrix()){
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function(){
    x
  }
  setInv<-function(invMatrix){
    inv<<-invMatrix
  }
  getInv<-function(){
    inv
  }
  list(set=set,get=get,setInv=setInv,getInv=getInv)
}

## Return a matrix that is the inverse of 'x'
cacheSolve<-function(x,...){
  inv<-x$getInv()
  if(is.null(inv)){
    mat<-x$get()
    inv<-solve(mat,...)
    x$setInv(inv)
  }
  else{
    message("getting cached inverse")
  }
  inv
}