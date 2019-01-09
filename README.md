## Caching-the-Inverse-of-a-Matrix
## Create a special matrix object that can cache its inverse
makeCacheMatrix<- function(m=funtion){
  i<-NULL 
  
 ##set the initial matrix
 set<-function(matrix){
   m<<-matrix
   i<<-NULL
   }
   
 ##get the matrix
 get<<-function(){
   m
   }
   
 ##set the inverse of the matrix
 setInverse<-function(inverse){
   i<<-inverse
   }
   
 ##get the inverse of the matrix
 getInverse<-function(){
   i
   }
   
 ##return a list of the above functions
 list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
 }
 
 ##This is the second part of the qusetion
 ##Compute the inverse of the special matrix returned in the first part. If the inverse has already been calculatedm and matrix 
 ##has not changed,then the"cacheSolve" should retrive the inverse from the cache.
 cacheSolve<-function(x,...){
   m<-x$getInverse(){
   if(!is.null(m){
     message("getting cached data")
     return(m)
     }
     
   ##get the matrix from our object
   data<-x$get()
   ##get inverse using matrix multiplication
   m<-solve(data) %*% data
   ##set the inverse to the object
   x$setInverse(m)
   ##return the matrix
   m
   }
   
  ##This is the end of the problem.
     
