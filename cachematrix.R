## makeCacheMatrix takes an argument matrix x, calculates it's inverse and stores the same in a global variable.
## The global variable "alpha" stores the matrix inverse for retrieval later on.

makeCacheMatrix <- function(x = matrix()) {
        
        xrow <<- nrow(x)
        xcol <<- ncol(x)          ## calculates number of rows and number of columns for x and stores in a global variable
        orgx <<- x              ## stores original matrix passed in global variable for comparison later
        yinv <- solve(x)
        alpha <<- yinv          ## Used to store the inverse value for retrieval later
        print(alpha)
}


## This function takes a new matrix "y2" and checks its value against matrix "orgx" by checking individual row and column
## values using 2 nested for loops and 1 if condition. If "y2" turns out to be same as "orgx", then "alpha" (stored inverse of "orgx") is printed
## else inverse of "y2" is calculated and stored in "alpha" replacing the original value obtained through "orgx" inversion.

cacheSolve <- function(y2 = matrix()) {
                
                yrow <<- nrow(y2)
                ycol <<- ncol(y2)
                init <- 0               ## variable initialized which will be used to check matrix equality
                ver <- yrow * ycol      ## variable initialized which will be used to check matrix equality
                beta <- y2
                
                if (yrow == xrow & ycol == xcol) {     ## loop will only run if both original matrix and current matrix rows & columns are equal
                        
                        for(i in yrow){
                                for (j in ycol){
                                        if (orgx[i,j] == beta[i,j]){
                                                init <- init + 1        ## 2 nested for loops and if condition to check individual values of both matrices
                                        } 
                                }
                        }
                        if (init == ver){
                                print(alpha)            ## if "init" = rows x columns, then both matrices are equal, hence inverse is also equal. Without calculation, "alpha" value is retrieved and printed
                        }
                        else {
                                alpha <<- solve(y2)     ## if "init" != rows x columns, then new "alpha"is calculated and stored in global environment
                                print(alpha)
                        }
                        
                        
                }
                
                else {
                        alpha <<- solve(y2)             ## if rows and columns are different, new "alpha" is calculated and stored in global environment.
                        print(alpha)
                }
                
        }
        
        ## Return a matrix that is the inverse of 'x'
