# Least Square Method Regression Analysis, Proof of Concept
#
# By..........................: Pedro Carneiro Jr., Goiania, GO, Brazil
# Spreadsheet concept.........: 2019-11-21
# Start of implementation in R: 2019-11-21
# 
# This piece of software is intended to demonstrate the Least Square Method
# Regression Analysis technique by using a conventional successive refinements
# approach. There is the possibility that this same objective can be implemented
# in less lines of code, anyhow this is more intended to be a "working software"
# then a "few lines of code software".
#
# Due o the used technique for achieving the intended results, the code
# readability was comprised. Refactoring the code may be considered in order to
# gain code readability. You are free to edit this code and make it look better.
################################################################################

# Input from keyboard
readVariables <- function(axisType) {
  # axisType = "X"
  returnVector <- vector(mode="numeric", length=0)
  while(T) {
    num <- readline(paste0("How many numbers do you want to enter for the ", axisType, " Axis?: "))
    num <- as.numeric(num)
    if (!is.na(num)) {
      countEntry <- 1
      while(num > 0) {
        num2 <- readline(paste0("Enter number #", countEntry, " (You have ", num, " left): "))
        returnVector <- c(returnVector, num2)
        num <- num - 1
        countEntry <- countEntry + 1
        # print(num2)
        # print(num)
      }  
      break
    }
  }
  return(returnVector)
}

# Generic function to combine any X, Y pair into a two-column vector.
# The number of lines of the resulting vector will be the number of lines of
# the biggest input vector.
combineXY <- function(x, y) {
  return(cbind(x, y))
} 

# Read the X and Y variables from the keyboard.
getVariablesXY <- function() {
  
  # Read X and Y variable pairs from keyboard.
  # x is a vector of numbers
  # y is a vector of numbers
  x <- as.numeric(readVariables("X"))
  y <- as.numeric(readVariables("Y"))
  return(combineXY(x, y))
}

# xyMean is the point where the regression line will have to pass.
# It is a two-elements, one line vector.
# Composed by xMean and yMean.
xyMean <- function(xMean, yMean) {
  return(combineXY(xMean, yMean))
} 

# Calculate the distance from a given n to its mean.
dn <- function(n) {
  # dn = n - mean(n)
  # dn is a vector of distances.
  return(n - mean(n))
}


########
# Main #
########

# Read the X and Y variables from the keyboard.
xy <- getVariablesXY()
x <- xy[,1]
y <- xy[,2]

# Calculate xMean. It is a single number (single element vector).
# It is the mean of the elements in the x vector.
xMean <- mean(x)

# Calculate yMean. It is a single number (single element vector).
# It is the mean of the elements in the y vector.
yMean <- mean(y)

# Calculate the distance from a given x to its mean.
dx <- dn(x)

# Calculate the distance from a given y to its mean.
dy <- dn(y)

# Calculate the (Method) square of a distance dx.
dxSquared <- dx ^ 2

# Calculate SumOf_dxSquared. It is a single number (single element vector).
# It is the sum of the elements in the dxSquared vector.
SumOf_dxSquared <- sum(dxSquared)

# Calculate the (Method) multiplication of two distances dx and dy.
dxTimesdy <- dx * dy

# Calculate SumOf_dxTimesdy. It is a single number (single element vector).
# It is the sum of the elements in the dxTimesdy vector.
SumOf_dxTimesdy <- sum(dxTimesdy)

# Calculate the (Method) square of a distance dy.
dySquared <- dy ^ 2

# Calculate SumOf_dySquared. It is a single number (single element vector).
# It is the sum of the elements in the dySquared vector.
SumOf_dySquared <- sum(dySquared)

### Now going for the R^2 ###

# b1 = Sum((dx) * (dy)) / Sum(dx^2)
b1 <- SumOf_dxTimesdy/SumOf_dxSquared

# b0 = Mean(y) - b1 * Mean(x)
b0 <- yMean - b1 * xMean

# Calculate the (Method) yHat - The regression line Ys - given b0, b1 and xHat.
# The yHats are the calculated regression line y parameters.
# X is fixed, in other words, the xHats are the actual X parameters.
yHat <- b0 + b1 * x

# Calculate the distance from yHat to yMean.
# dyy is the vector of distances from the regression line Ys to the yMean.
dyy <- yHat - yMean

# Calculate the (Method) square of the distance from yHat to yMean.
# dyy^2 = (yHat - yMean)^2
dyySquared <- dyy ^ 2

# Calculate SumOf_dyySquared. It is a single number (single element vector).
# It is the sum of the elements in the dyySquared vector.
SumOf_dyySquared <- sum(dyySquared)

# R^2 itself
# R^2 = Sum(dyy^2) / Sum(dy^2)
rSquared <- SumOf_dyySquared/SumOf_dySquared

# xyMean, combineXY, etc will be used for plotting... ???
regressionLineXY <- combineXY(x, yHat)         # Combines x and y into a single vector which represents the regression line.
b <- combineXY(0, b0)                          # This is where the regression line crosses the y axis.
regressionLineXY <- rbind(regressionLineXY, b) # Adds the (x=0, y=b0) coordinaes to the regression line to be plotted.

# Plot it all
plot(xy, col="blue", pch=1, cex=1.5)          # Plot the (x, y) points.
#                                               pch = 0, square
#                                               pch = 1, circle (default)
#                                               pch = 2, triangle point up
#                                               pch = 3, plus
#                                               pch = 4, cross
#                                               pch = 5, diamond
#                                               pch = 6, triangle point down
#                                               pch = 7, square cross
#                                               pch = 8, asterisk
#                                               pch = 9, diamond plus
#                                               pch = 10, circle plus
#                                               pch = 11, triangles up and down
#                                               pch = 12, square plus
#                                               pch = 13, circle cross
#                                               pch = 14, square and triangle down
#                                               pch = 15, filled square
#                                               pch = 16, filled circle
#                                               pch = 17, filled triangle point-up
#                                               pch = 18, filled diamond
#                                               pch = 19, solid circle
#                                               pch = 20, bullet (smaller circle)
#                                               pch = 21, filled circle blue
#                                               pch = 22, filled square blue
#                                               pch = 23, filled diamond blue
#                                               pch = 24, filled triangle point-up blue
#                                               pch = 25, filled triangle point down blue
# The following arguments can be used to change the color and the size of the points :
# col: color (code or name) to use for the points
# bg: the background (or fill) color for the open plot symbols. It can be used only when pch = 21:25.
# cex: the size of pch symbols
# lwd: the line width for the plotting symbols
####
points(combineXY(xMean, yMean), col="purple", cex=2, lwd=2) # Plot the crossing of the means of x and y.
abline(v=xMean, col="purple")                 # Plot the x mean.
abline(h=yMean, col="purple")                 # Plot the y mean.
lines(regressionLineXY, col="green")          # Plots the regression line.
points(regressionLineXY, col="green", cex=0.7, lwd=1) # Plot the points of the regression line (x, yHat).

# FIM
