## this code contains two functions. Together they can be used to calculate and output the inverse of a matrix. 
##The advantage of using these two functions instead of just the solve() is they will cache the inverse of matrices that
##have already been evaulated so r will not need to recompute that value 

## the makeCachematrix is used to take in a square matrix and creates a list of 4 functions which do the following
##set the values of matrix, get the values of a matrix, set the inverse and get the inverse  )

makeCacheMatrix <- function(x = matrix())
{
  inv <- NULL               ## creates a variable inv and sets its initial value to null 
  
  set <- function(y)        ## this creates the function set() which is used to set variables 
                            ##inside the makeCacheMatrix function
  {                         ## y is a numeric argument passed into makeCacheMatrix     
    x <<- y                 ## set x for the function enviroment equal to y 
    inv <<- NULL            ## set inv for the function enviroment equal to null      
  }
  get <- function()         ## this creates the get() function within makeCacheMatrix and assigns a matrix to it
  {x}
  
  setinv <- function(solve)        ## this creates the setinv() funciton which takes the inverse matrix 
  {inv <<- solve}                  ## and assigns that to the inv argument in the makeCacheMatrix frane 
  
  getinv <- function()             ## this creates the getinv() function which returns the inv argument 
  {inv}
  
  list(set = set, get = get,       ##creates a list of all the valves of the fucnctions in the 
       setinv = setinv,            ## makeMatrix frame 
       getinv = getinv)
  
  
}


## the cacheSolve function  calculates the inverse of a matrix using the fuciton above. Before it does this calculation
## it checks to see if this invserse has already been calculated and store. If it has, then it will pull the cached 
##inverse and return that. If it has not been calcualted it will then calcuate it and return the inverse. 

cacheSolve <- function(x, ...) 
    
{
  inv <- x$getinv()        ## goes into the x enviroment and assigns the inv values 
                           ## from that enviroment to this one
  
  if(!is.null(inv))        ## checks to see if there is anything stored or cached in the inv argument  
  {                        ## if there is a stored value it pulls that infomraiton and returns it 
    message("getting cached data")
    return(inv)
  }
                           ## if there is no cached value this function will then
                           ##calculate the inverse useing the next steps 
  data <- x$get()          ## this pulls the data from within the x matrix 
  inv <- solve(data, ...)  ## this solves for the inverse of the matrix by calling the solve function 
  x$setinv(inv)            ## assigns the inverse using the setinv() function 
  inv                      ##Returns a matrix that is the inverse of 'x'
} 
  

