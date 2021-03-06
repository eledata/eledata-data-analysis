% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formattable.R
\name{format_table}
\alias{format_table}
\title{Format a data frame with formatter functions}
\usage{
format_table(x, formatters = list(), format = c("markdown", "pandoc"),
  align = "r", ..., row.names = rownames(x), check.rows = FALSE,
  check.names = FALSE)
}
\arguments{
\item{x}{a \code{data.frame}.}

\item{formatters}{a list of formatter functions or formulas.
The existing columns of \code{x} will be applied the formatter
function in \code{formatters} if it exists.

If a formatter is specified by formula, then the formula will be
interpreted as a lambda expression with its left-hand side being
a symbol and right-hand side being the expression using the symbol
to represent the column values. The formula expression will be evaluated
in \code{envir}, that, to maintain consistency, should be the calling
environment in which the formula is created and all symbols are defined
at runtime.}

\item{format}{The output format: markdown or pandoc?}

\item{align}{The alignment of columns: a character vector consisting
of \code{'l'} (left), \code{'c'} (center), and/or \code{'r'} (right).
By default, all columns are right-aligned.}

\item{...}{additional parameters to be passed to \code{knitr::kable}.}

\item{row.names}{row names to give to the data frame to knit}

\item{check.rows}{if TRUE then the rows are checked for consistency
of length and names.}

\item{check.names}{\code{TRUE} to check names of data frame to make
valid symbol names. This argument is \code{FALSE} by default.}
}
\value{
a \code{knitr_kable} object whose \code{print} method generates a
string-representation of \code{data} formatted by \code{formatter} in
specific \code{format}.
}
\description{
This is an table generator that specializes in creating
formatted table presented in a mix of markdown/reStructuredText and
HTML elements. To generate a formatted table, each column of data
frame can be transformed by formatter function.
}
\examples{
# mtcars (mpg in red)
format_table(mtcars,
   list(mpg = formatter("span", style = "color:red")))

# mtcars (mpg in red if greater than median)
format_table(mtcars, list(mpg = formatter("span",
   style = function(x) ifelse(x > median(x), "color:red", NA))))

# mtcars (mpg in red if greater than median, using formula)
format_table(mtcars, list(mpg = formatter("span",
   style = x ~ ifelse(x > median(x), "color:red", NA))))

# mtcars (mpg in gradient: the higher, the redder)
format_table(mtcars, list(mpg = formatter("span",
   style = x ~ style(color = rgb(x/max(x), 0, 0)))))

# mtcars (mpg background in gradient: the higher, the redder)
format_table(mtcars, list(mpg = formatter("span",
   style = x ~ style(display = "block",
   "border-radius" = "4px",
   "padding-right" = "4px",
   color = "white",
   "background-color" = rgb(x/max(x), 0, 0)))))

# mtcars (mpg in red if vs == 1 and am == 1)
format_table(mtcars, list(mpg = formatter("span",
    style = ~ style(color = ifelse(vs == 1 & am == 1, "red", NA)))))
}

