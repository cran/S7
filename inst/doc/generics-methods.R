## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(S7)

## -----------------------------------------------------------------------------
mean <- new_generic("mean", "x")
method(mean, class_numeric) <- function(x) sum(x) / length(x)

## ---- error = TRUE, eval = FALSE----------------------------------------------
#  mean(100, na.rm = TRUE)

## -----------------------------------------------------------------------------
method(mean, class_numeric) <- function(x, na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }

  sum(x) / length(x)
}
mean(c(100, NA), na.rm = TRUE)

## -----------------------------------------------------------------------------
simple_print <- new_generic("simple_print", "x")
method(simple_print, class_double) <- function(x, digits = 3) {}
method(simple_print, class_character) <- function(x, max_length = 100) {}

## -----------------------------------------------------------------------------
method(simple_print, class_list) <- function(x, ...) {
  for (el in x) {
    simple_print(el, ...)
  }
}

## ---- error = TRUE, eval = FALSE----------------------------------------------
#  simple_print(list(1, 2, 3), digits = 3)
#  simple_print(list(1, 2, "x"), digits = 3)

## -----------------------------------------------------------------------------
method(simple_print, class_double) <- function(x, ..., digits = 3) {}
method(simple_print, class_character) <- function(x, ..., max_length = 100) {}

simple_print(list(1, 2, "x"), digits = 3)

## -----------------------------------------------------------------------------
simple_print(list(1, 2, "x"), diggits = 3)

## ---- eval = FALSE------------------------------------------------------------
#  length <- new_generic("length", "x", function(x) {
#    S7_dispatch()
#  })

## -----------------------------------------------------------------------------
display <- new_generic("display", "x")
S7_data(display)

## -----------------------------------------------------------------------------
foo <- new_generic("foo", "x", function(x, y, ...) {
  S7_dispatch()
})

## -----------------------------------------------------------------------------
method(foo, class_integer) <- function(x, ...) {
  10
}

## -----------------------------------------------------------------------------
mean <- new_generic("mean", "x", function(x, ..., na.rm = TRUE) {
  S7_dispatch()
})
method(mean, class_integer) <- function(x, na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  sum(x) / length(x)
}

## -----------------------------------------------------------------------------
method(mean, class_double) <- function(x, na.rm = FALSE) {}
method(mean, class_logical) <- function(x) {}

## -----------------------------------------------------------------------------
mean <- new_generic("mean", "x", function(x, ..., na.rm = TRUE) {
  if (!identical(na.rm, TRUE) && !identical(na.rm = FALSE)) {
    stop("`na.rm` must be either TRUE or FALSE")
  }
  S7_dispatch()
})

## -----------------------------------------------------------------------------
mean <- new_generic("mean", "x")
method(mean, class_numeric) <- function(x) {
  sum(x) / length(x)
}
mean(1:10)

## -----------------------------------------------------------------------------
date <- new_class("date", parent = class_double)
# Cheat by using the existing base .Date class
method(print, date) <- function(x) print(.Date(x))
date(c(1, 10, 100))

## -----------------------------------------------------------------------------
method(mean, date) <- function(x) {
  date(mean(super(x, to = class_double)))
}
mean(date(c(1, 10, 100)))

## -----------------------------------------------------------------------------
pet <- new_class("pet")
dog <- new_class("dog", pet)
cat <- new_class("cat", pet)

language <- new_class("language")
english <- new_class("english", language)
french <- new_class("french", language)

speak <- new_generic("speak", c("x", "y"))
method(speak, list(dog, english)) <- function(x, y) "Woof"
method(speak, list(cat, english)) <- function(x, y) "Meow"
method(speak, list(dog, french)) <- function(x, y) "Ouaf Ouaf"
method(speak, list(cat, french)) <- function(x, y) "Miaou"

speak(cat(), english())
speak(dog(), french())

# This example was originally inspired by blog.klipse.tech/javascript/2021/10/03/multimethod.html
# which has unfortunately since disappeaed.

