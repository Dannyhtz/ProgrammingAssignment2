## makeCacheMatrix creates a parent function of a matrix, that allows to save
## inverse valor. It returns a list of functions which allows another functions
## edit the parent function

makeCacheMatrix <- function(x = matrix()) {
  
  inverse<-NULL ##It is where inverse matrix will be saved
  ##--------------matrix functions----------------
  setmatrix<-function(newmatrix=matrix()) { ##To set a new matrix value
      x<<-newmatrix ##New matrix is now our x
      inverse<<-NULL ##We need to drop any old value for the inverse
      }
  getmatrix<-function(){x} #For get the original matrix
  ##----------------inverse functions----------------
  setinverse<-function(newinverse){inverse<<-newinverse} # To establish the inverse matrix
  getinverse<-function(){inverse} #To get the inverse matrix valor
  ##--------------The function will return a list of functions-----
  list(getmatrix=getmatrix, 
       setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve takes a makeCacheMatrix matrix's son and calculates its inverse
## Then it saves the value in its parent to be more efficient. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse<-x$getinverse() ##Get inverse value of parent's, if there are any
  
  if (!is.null(inverse)){ ##If there are any it returns it
      return(inverse)
  }
  ##If not it calculates it, save it in the parent value and return it
  matrix<-x$getmatrix()
  inverse <- solve(matrix)
  x$setinverse(inverse)
  return(inverse)
}
###Just a TRY
mat<-matrix(1:4,nrow=2,ncol=2)
mat_created<-makeCacheMatrix(mat)
cached<-cacheSolve(mat_created)
real_inverse<-solve(mat)
identical(cached,real_inverse)
