## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(S7)

## -----------------------------------------------------------------------------
Foo := new_class(package = "Bar")
class(Foo())

`mean.Bar::Foo` <- function(x, ...) {
  "mean of foo"
}

mean(Foo())

## -----------------------------------------------------------------------------
foo := new_generic("x")

# Default method dispatches to S3
method(foo, class_any) <- function(x, ...) {
  UseMethod("foo")
}

## -----------------------------------------------------------------------------
# Define the "true" default method
foo.default <- function(x) paste0(x, " + 10")

## -----------------------------------------------------------------------------
rle <- function(x) {
  if (!is.vector(x) && !is.list(x)) {
    stop("'x' must be a vector of an atomic type")
  }
  n <- length(x)
  if (n == 0L) {
    Rle(integer(), x)
  } else {
    y <- x[-1L] != x[-n]
    i <- c(which(y | is.na(y)), n)
    Rle(diff(c(0L, i)), x[i])
  }
}
Rle <- function(lengths, values) {
  structure(
    list(
      lengths = lengths,
      values = values
    ),
    class = "Rle"
  )
}

## -----------------------------------------------------------------------------
Rle := new_class(
  parent = class_list,
  constructor = function(lengths, values) {
    new_object(list(lengths = lengths, values = values))
  }
)
rle(1:10)

## -----------------------------------------------------------------------------
Rle := new_class(properties = list(
  lengths = class_integer,
  values = class_atomic
))

## -----------------------------------------------------------------------------
method(`$`, Rle) <- prop
inverse.rle(rle(1:10))

## -----------------------------------------------------------------------------
methods::setClass("S4Point",
  slots = c(x = "numeric", y = "numeric")
)

ColoredPoint <- new_class("ColoredPoint",
  parent = methods::getClass("S4Point"),
  properties = list(color = class_character)
)

pt <- ColoredPoint(x = 1, y = 2, color = "red")
pt@x
pt@color
methods::is(pt, "S4Point")

## -----------------------------------------------------------------------------
methods::setGeneric("radius", function(x) standardGeneric("radius"))
methods::setMethod("radius", "S4Point", function(x) {
  sqrt(x@x^2 + x@y^2)
})

radius(pt)

## -----------------------------------------------------------------------------
pt <- methods::initialize(pt, x = 3, color = "blue")
pt@x
pt@color

## -----------------------------------------------------------------------------
Sample <- new_class("Sample",
  properties = list(
    name = class_character,
    score = class_numeric
  ),
  validator = function(self) {
    if (length(self@score) != 1) {
      "@score must have length 1"
    } else if (self@score < 0) {
      "@score must be non-negative"
    }
  }
)

S4_register(Sample)
Sample_S4 <- S4_contains(Sample)
methods::setClass("ReplicatedSample",
  contains = Sample_S4,
  slots = c(replicate = "integer")
)

rs <- methods::new("ReplicatedSample",
  name = "A",
  score = 3,
  replicate = 1L
)

isS4(rs)
rs@name
prop(rs, "score")
methods::validObject(rs)

## -----------------------------------------------------------------------------
sample_label <- new_generic("sample_label", "x")
method(sample_label, Sample) <- function(x) {
  paste0(x@name, ": ", x@score)
}

sample_label(rs)

## -----------------------------------------------------------------------------
Class1 := new_class()
Class2 := new_class()
Union1 <- new_union(Class1, Class2)

foo := new_generic("x")
method(foo, Union1) <- function(x) ""
foo

