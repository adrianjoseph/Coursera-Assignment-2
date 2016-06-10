## The combination of these functions are meant to find the inverse of a 
## matrix argument and cache the results in temporary memory such that 
## the results can be called without using main memory


## The makeCacheMatrix() function takes a matrix argument and defines the actions of 4 
## functions, which either make its argument accessible, via a getarg() function, 
## globally assigns the argument of the setmatrix() function and finally allows the 
## getmatrix() function to return the available inverse matrix object,
## all via a list argument call from another funtion
makeCacheMatrix <- function(x = matrix()) {
    matrixobj<-NULL#creates a globally accessible object with no content
    stage<-function(y){
        x<<-y#uses the super assignment function to make the 'stage' function argument
        #accessible to all functions in makeCacheMatrix
        matrixobj<<-NULL
    }
    getarg<-function(){
        x
    }
    setmatrix<-function(argsolve){
        matrixobj<<-argsolve#gets its argument from cachesolve call to solve(ans)
    } 
    getmatrix<-function(){
        matrixobj#this object is being sourced from the output from the setmatrix() 
        #function as a result of the golbal assignment
    }
    list(stg=stage,arg=getarg,setma=setmatrix,getma=getmatrix)#creates a list of functions
    #which can be called
}


## The cacheSolve() funtion assigns the matrix currently available in the getmatrix()
## function call and checks to see if the object is null or not. If the object is not 
## null it returns the results. If the matrix object from the getmatrix() function call
## is not null, it calculates the inverse of the matrix provided in the makeCacheMatrix()
## function, and stores the results within the makeCacheMatrix() function via the
## setmatrix() function
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    matrixobj<-x$getma()#calls the getmatrix() function within the makeCacheMatrix() function
    if(!is.null(matrixobj)){#checks to see if anything is cached
        message("pulling cached data")
        return(matrixobj)
    }
    message("calculating...")
    ans<-x$arg()#calls the getarg() function within the makeCacheMatrix() function
    matrixobj<-solve(ans)
    x$setma(matrixobj)#calls the setmatrix() function within the makeCacheMatrix() function
    matrixobj
}
