
# Function that calculates the integer part of a floating point number.
# Handles positive and negative numbers.

IntPart <- function(x)
{
  y <- floor(x)

  return (y)
}


# Function that calculates the fractional part of a floating point number.
# Handles positive and negative numbers.

FracPart <- function(x)
{
  y <- 0
  if (x <= 0) {
    y <- x - IntPart(x)
  } else {
    y <- abs(x) - IntPart(abs(x))
  }

  return (y)
}

# x is a vector of length 3
# y is a vector of length 3
DotProduct <- function(x, y)
{
  return(x%*%y)
}

# x is a vector of length 3
# y is a vector of length 3
CrossProduct <- function(x,y)
{
  result <- c(0.0, 0.0, 0.0)
  for (i in 1:length(x)){
    result[1] <- x[2] * y[3] - x[3] * y[2]
    result[2] <- x[3] * y[1] - x[1] * y[3]
    result[3] <- x[1] * y[2] - x[2] * y[1]
  }
  
  return(result)
}

# Calculate the magnitude of a vector x of length 3
VecNorm <- function(x)
{
  return(sqrt(x[1] * x[1] + x[2] * x[2] + x[3] * x[3]))
}

# x is a vector of length 3
UnitVector <- function(x)
{
  mag <- VecNorm(x)
  result <- c(0.0, 0.0, 0.0)
  result[1] <- x[1] / mag
  result[2] <- x[2] / mag
  result[3] <- x[3] / mag
  
  return (result)
}

# Reduce a to the range 0 <= a < b
Amodulo <- function(a, b)
{
  return(a - b * floor(a/b))
}

# Returns a rotation matrix based on the axis (x, y, or z) and the angle phi
# The angle phi is in radians
RotationMatrix <- function(axis, phi)
{
  mat <- matrix(0.0, nrow = 3, ncol = 3)
  cosphi <- cos(phi)
  sinphi <- sin(phi)
  
  if (axis == 1) {
    mat[1,1] <- 1.0
    mat[2,2] <- cosphi
    mat[3,3] <- cosphi
    mat[2,3] <- sinphi
    mat[3,2] <- -sinphi
  } else if (axis == 2) {
    mat[1,1] <- cosphi
    mat[1,3] <- -sinphi
    mat[2,2] <- 1.0
    mat[3,1] <- sinphi
    mat[3,3] <- cosphi
  } else if (axis == 3){
    mat[1,1] <- cosphi
    mat[2,2] <- cosphi
    mat[3,3] <- 1.0
    mat[2,1] <- -sinphi
    mat[1,2] <- sinphi
  } else {print("Axis is wrong value")}
  
  return (mat)
}

# Interpolate three values using the interpolating value n
Interpolate <- function(vec, n)
{
  # Take differences
  a <- vec[2] - vec[1]
  b <- vec[3] - vec[2]
  c <- b - a
  
  y <- vec[2] + n/2 * (a + b + n * c)
  
  return(y)
}

