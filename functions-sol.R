## Aufgabe functions d.

#' function for xor-operation
#' @param x logical vector
#' @param y logical vector
#' 
#' @result logical vector generating output for xor-operation
`%xor%` <- function(x, y) {
  (x | y) & !(x & y)
}

## Aufgabe functions f.

a <- rnorm(100, 0, 1)
b <- rnorm(100, 1, 2)

#' function to open and close pdf-device, that calls plot() with given arguments
#' @param x numerical Vector
#' @param y optional numerical vector
#' @param pdf_args a list that contains a parameterlist for the function pdf()
#' ?pdf() to see options 
#' @param ... additional arguments to be passed to plot()
#' 
#' @return a pdf-file containing the created plot
pdf_plotting <- function(x, y = NULL, pdf_args = NULL, ...) {
  checkmate::assert_numeric(x)
  checkmate::assert_numeric(y, null.ok = TRUE)
  checkmate::assert_list(pdf_args)
  on.exit(dev.off())
  do.call(pdf, pdf_args)
  plot(x, y, ...)
}

pdf_plotting(x = a, y = b, pdf_args = list())
pdf_plotting(x = a, y = b, pdf_args = list(width = 9, height = 9))
pdf_plotting(x = a, y = b, pdf_args = list(), main = "Testplot", xlab = "x-axis"
             , col = "red")
pdf_plotting(x = iris$Sepal.Length, pdf_args = list(file = "testplot.pdf")
             , col = "red")

# Leider verstehe ich nicht so ganz, wieso der letzte Aufruf einen Plot
# generiert mit "y" als Bezeichnung der y-Achse statt "iris$Sepal.Length"
# Das wäre nämlich der Fall, wenn man direkt plot(x = iris$Sepal.Length) aufruft
pdf_plotting(x = iris$Sepal.Length, pdf_args = list(file = "testplot2.pdf")
             , type = "b")
