

makeCacheMatrix <- function(x = matrix()) {#initialising as null
  i<-NULL            
  s<-function(y)
  {
    x<<-y
    i<<-NULL
  }
  
  #obtaining the matrix 
  
  ob<-function()x            
  si<-function(inverse)i<<-inverse
  gi<-function()
  { 
    ir<-ginv(x)
    
    ir%*%x           
  }
  
  
  list(si = si, s = s 
       gi = gi, g = g)


}


cacheSolve <- function(x, ...) {
  #checking for null 
  
  i<-x$gi()                  
  if(!is.null(i))
  {                 
    message("obtaining cached data")
    
    
    #returning inverse value 
    
    return(i)                       
  }
  
  info<-x$ob()
  #finding the inverse 
  
  i<-solve(info,...)        
  
  x$si(i)
  #returning the inverse of the given matrix 
  i   
}
}
