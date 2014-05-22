# TODO: Add comment
# 
# Author: pacha
###############################################################################

#Question 1
#Suppose I define the following function in R

cube <- function(x, n) {
	x^3
}

#What is the result of running 

cube(3)

#Question 2
#The following code will produce a warning in R.

x <- 1:10
if(x > 5) {
	x <- 0
}

#Why?

#la sintaxis está mala, una posibilidad (no se puede saber q pide pq el comando está "roto") sería:
x <- 1:10 # Creates sample data
ifelse(x<5 | x>8, x, 0)

#el código está malo pq if es una función logica con vectores unidimensionales

#Question 3
#Consider the following function

f <- function(x) {
	g <- function(y) {
		y + z
	}
	z <- 4
	x + g(x)
}

#If I then run in R

z <- 10
f(3)

#Question 4
#Consider the following expression:
		
x <- 5
y <- if(x < 3) {
			NA
		} else {
			10
		}

#What is the value of 'y' after evaluating this expression?

y

#Question 5
#Consider the following R function

h <- function(x, y = NULL, d = 3L) {
	z <- cbind(x, d)
	if(!is.null(y))
		z <- z + y
	else
		z <- z + f
	g <- x + y / z
	if(d == 3L)
		return(g)
	g <- g + 10
	g
}

#Which symbol in the above function is a free variable?

# f pues no aparece dentro de function()

#Question 6
#What is an environment in R?

#a collection of symbol/value pairs

#Question 7
#The R language uses what type of scoping rule for resolving free variables?

#lexical scoping

#Question 8
#How are free variables in R functions resolved?

#The values of free variables are searched for in the environment in which the function was defined

#Question 9
#What is one of the consequences of the scoping rules used in R?

#All objects must be stored in memory

#Question 10
#In R, what is the parent frame?

#It is the environment in which a function was called