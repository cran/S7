% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generic.R, R/method-dispatch.R
\name{new_generic}
\alias{new_generic}
\alias{S7_dispatch}
\title{Define a new generic}
\usage{
new_generic(name, dispatch_args, fun = NULL)

S7_dispatch()
}
\arguments{
\item{name}{The name of the generic. This should be the same as the object
that you assign it to.}

\item{dispatch_args}{A character vector giving the names of one or more
arguments used to find the method.}

\item{fun}{An optional specification of the generic, which must call
\code{S7_dispatch()} to dispatch to methods. This is usually generated
automatically from the \code{dispatch_args}, but you may want to supply it if
you want to add additional required arguments, omit \code{...}, or perform
some standardised computation in the generic.

The \code{dispatch_args} must be the first arguments to \code{fun}, and, if present,
\code{...} must immediately follow them.}
}
\value{
An S7 generic, i.e. a function with class \code{S7_generic}.
}
\description{
A generic function uses different implementations (\emph{methods}) depending on
the class of one or more arguments (the \emph{signature}). Create a new generic
with \code{new_generic()} then use \link{method<-} to add methods to it.

Method dispatch is performed by \code{S7_dispatch()}, which must always be
included in the body of the generic, but in most cases \code{new_generic()} will
generate this for you.

Learn more in \code{vignette("generics-methods")}
}
\section{Dispatch arguments}{

The arguments that are used to pick the method are called the \strong{dispatch
arguments}. In most cases, this will be one argument, in which case the
generic is said to use \strong{single dispatch}. If it consists of more than
one argument, it's said to use \strong{multiple dispatch}.

There are two restrictions on the dispatch arguments: they must be the first
arguments to the generic and if the generic uses \code{...}, it must occur
immediately after the dispatch arguments.
}

\examples{
# A simple generic with methods for some base types and S3 classes
type_of <- new_generic("type_of", dispatch_args = "x")
method(type_of, class_character) <- function(x, ...) "A character vector"
method(type_of, new_S3_class("data.frame")) <- function(x, ...) "A data frame"
method(type_of, class_function) <- function(x, ...) "A function"

type_of(mtcars)
type_of(letters)
type_of(mean)

# If you want to require that methods implement additional arguments,
# you can use a custom function:
mean2 <- new_generic("mean2", "x", function(x, ..., na.rm = FALSE) {
   S7_dispatch()
})

method(mean2, class_numeric) <- function(x, ..., na.rm = FALSE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  sum(x) / length(x)
}

# You'll be warned if you forget the argument:
method(mean2, class_character) <- function(x, ...) {
  stop("Not supported")
}
}
\seealso{
\code{\link[=new_external_generic]{new_external_generic()}} to define a method for a generic
in another package without taking a strong dependency on it.
}
