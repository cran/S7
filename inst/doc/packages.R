## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(S7)

## -----------------------------------------------------------------------------
.onLoad <- function(...) {
  S7::S7_on_load()
}

.onUnload <- function(...) {
  S7::S7_on_unload()
}

S7::S7_on_build()

## ----eval = FALSE-------------------------------------------------------------
# TheirClass <- new_external_class("theirpkg", "TheirClass")
# method(my_generic, TheirClass) <- function(x) { ... }
# 
# their_generic <- new_external_generic("theirpkg", "their_generic", "x")
# method(their_generic, MyClass) <- function(x) { ... }

## -----------------------------------------------------------------------------
Foo := new_class(package = "S7")
`length.S7::Foo` <- function(x) 10
length(Foo())

