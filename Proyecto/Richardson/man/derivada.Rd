\name{derivada}
\alias{derivada}
\title{derivada(x, y)}
\usage{
derivada(x, y, h = 0.01, n = 3)
}
\description{
Calculates the derivative of the data x from the data x and y
}
\examples{
x = seq(0, 1, by = 0.1)
y = seq(0, 1, by = 0.1)
derivada(x, y, h = 0.01, n = 3)
}
\value{
Derived from the x values and error calculation
}
\arguments{
\item{x}{
List of values of x in a function
}
\item{y}{
List of values of y in a function
}
\item{h}{
Value of the increase
}
\item{n}{
Number of iterations
}
}
\author{Diego Barajas "barajas-d@javeriana.edu.co and Brandonn Cruz "Brandonn_Cruz@javeriana.edu.co"}

