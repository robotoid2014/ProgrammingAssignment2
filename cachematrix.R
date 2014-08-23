########################################
## Class:       Coursera R Programming.
## Assignment:  Programming Assignment #2.
## Filename:    cachematrix.R.
## Date:        8/22/2014
## Summary:     Cache the inverse of a matrix by creating the following functions:
##                      makeCacheMatrix() 
##                      cacheSolve() 
##
## Details of assignment:
## 1.   As examples, the instructor gives functions makeVector() and cachemean().
## 2.   R's solve() function computes the inverse of a square invertible matrix.
## 3.   Assume the matrix supplied is always an invertible matrix.
## 4.   There may be some benefit to caching rather than computing repeatedly,
##      since matrix inversion is a costly computation.
## 5.   The first function, makeCacheMatrix(), creates a vector that consists
##      of a list of child functions, detailed later.
## 6.   The second function, cacheSolve(), calls makeCacheMatrix's child functions 
##      to compute the inverse, detailed later.
## 7.   The operator "<<-" searches for the variable through parent environments, one level at a time. 
##      If found, then the variable's value is redefined. 
##      If not found, then the variable is created in the global environment and
##      assigned the value there, in the global environment.
##      The operator "<-" only assigns a value to a local variable.
## 8.   Lexical scoping in R means that the values of free variables are searched for
##      in the environment in which the function was defined.  (Slide 7/24 Scoping)
##      A free variable is one that's not defined locally.
## 
## Notes:
## 1.   Parent environment: The parent environment in R is the calling environment.  
##      If a function is defined inside another function, then the outer function is the parent of
##      the inside function.  For example, f_get()'s parent, defined below, is makeCacheMatrix().
##
##      More on environments: Each function has a local environment.
##      Objects created via "<-" in a local environment are not automatically accessible to objects outside
##      of that local environment.
##      To access another's environment:
##      Operator "<<-" can assign a value to a variable in one of its parent's environments.  See #7 above.
##      Syntax r$c() can call a child function from its parent function,
##      if the parent function returned the children as a list.  See #3 below.
##      
## 2.   Re-using object names can be confusing.
##      The instructor's example functions re-use object names in several locations.
##      Whenever this re-use of names occurred, the possiblity of confusion arises.
##      Hence, I renamed some objects for clarity.
##      a.      Arguments x to two main functions are different.  (New m_arg & f1)
##      b.      Variables m in two main functions are different.  (New m & m_inverse)
##      c.      Function names in first main function are different from list element names.
##              (New f_*.)
##
## 3.   A child function can be executed from outside its parent.
##      First, suppose a parent function creates functions inside of itself (child functions).
##      Next, the parent function returns a vector list of its child functions.
##      Then, outside the parent (the parent's parent or calling environment), use syntax 
##              r$c() where
##              r = value returned from calling parent function 
##                      (e.g. r <- parentFunction().
##              c = child function's name included in parent's vector list.
##
## 4.   Location of the caching of the matrix's inverse:
##      The inverse is cached in variable m inside the makeCacheMatrix() environment.
##      The object m is created by makeCacheMatrix(), 
##      and assigned via "<<-" operator by the child setinverse() function.
##
## 5.   How to execute this assignment:
##      The value of the 1st function (a list) is the argument to the 2nd function.
##      Note that m and m_inverse are not defined by the calling environment (command prompt here).
##      (The x arguments demonstrated in the instructor's examples are different from each other.)
##      For example:
##              cacheSolve(makeCacheMatrix(m_arg))
##      Here's a simple example to test at the command prompt:
##
## setwd("[fill in working directory]")
## rm(list=ls())
## source("cachematrix.R")
## m_arg <- matrix(1:4, 2, 2)
## f1 <- makeCacheMatrix(m_arg)
## f2 <- cacheSolve(f1)
## 
## Results:
## 
## > f2
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## 
## > solve(m_arg)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## 
## > cacheSolve(f1)
## getting cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## 
## > m
## Error: object 'm' not found
## 
## > m_inverse
## Error: object 'm_inverse' not found
## 
## > names(f1)
## [1] "get"        "setinverse" "getinverse"
########################################
##  
##  
##  
########################################
## Function:    makeCacheMatrix(x = matrix())
##
## Summary:      This function returns a list of child functions, described in more detail below, 
## that can be called individually to: 
## 1.   Return an object (invertible matrix) from parent's argument m_arg.
## 2.   Return the cache of an object (inverse) from parent m.
## 3.   Cache an object (inverse) into parent m.
##
## Details:
## Create m here and set to NULL.
##
## Create the following functions inside this parent function, and return the child funcions as a list.
## 1.   f_set()  
##      Comment out this function since it is not called in this assignment.
##      The parent environment of f_set() is makeCacheMatrix.  
##      In makeCacheMatrix, the variable m is defined and already assigned a value of NULL.
##      f_set() nulls out this parent m with the <<- operator.
##      Variable y is a free variable and is not used in this assignment.
##
##      ## f_set <- function(y) {
##      ## x1 <<- y
##      ## m <<- NULL
##      ## }
##
## 2.   f_get()
##      Returns m_arg, the argument variable from its parent environment makeCacheMatrix.
##
## 3.   f_setinverse() 
##      The argument of f_setinverse() is m_inverse.
##      This function assigns (<<-) the parent makeCacheMatrix m the value of m_inverse.
##      Note that any possible m created outside of makeCacheMatrix is different than 
##      the m created inside makeCacheMatrix, and will not be changed.
##
## 4.   f_getinverse()
##      This function returns m that is defined in its parent environment makeCacheMatrix.
########################################
makeCacheMatrix <- function(m_arg = matrix())
{
        m <- NULL
        
        f_get <- function() {m_arg}
        
        f_setinverse <- function(m_inverse) {m <<- m_inverse}
        
        f_getinverse <- function() {m}
        
        list(get = f_get,
             setinverse = f_setinverse,
             getinverse = f_getinverse)
}

########################################
## Function:    cacheSolve()
##
## Summary:     This function checks to determine if the inverse of the matrix,
## the argument to makeCacheMatrix()), has already been cached.  
## If the inverse was cached, return "getting cached data", and then return the inverse.
## If the inverse was not cached yet, cache it and then return the inverse.
## This function individually calls the child functions in makeCacheMatrix().
##
## Details:
## As the argument, pass the list of names of child functions returned by makeCacheMatrix().
## Call a child function (named getinverse()) in makeCacheMatrix() to retrieve
## the cached inverse.  Create m_inverse here and assign it this value.
## If the cached value is not null, display message "getting cached data" then leave this function.
## If the cached value is null, 
## 1.   Call a child function (named get()) in makeCacheMatrix() to retrieve 
##      the matrix passed as the argument.
## 2.   Caculate the inverse using R's solve().
## 3.   Call a child function (named setinverse()) in makeCacheMatrix() to cache the inverse.
## 4.   Return the inverse as m_inverse.
########################################
cacheSolve <- function(f1, ...) 
{
        m_inverse <- f1$getinverse()
        
        if(!is.null(m_inverse))
        {
                message("getting cached data")
                return(m_inverse)
        }
        
        data <- f1$get()
        
        m_inverse <- solve(data, ...)
        
        f1$setinverse(m_inverse)
        
        m_inverse
}

########################################
## eof (end of file
########################################
