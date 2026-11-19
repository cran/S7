## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(S7)

## -----------------------------------------------------------------------------
# yourPackage, version 1
gen := new_generic("x")

# depPackage's method, written against version 1
BClass := new_class()
method(gen, BClass) <- function(x, ...) "old"

# yourPackage, version 2 adds `verbose`
gen := new_generic("x", fun = function(x, verbose = FALSE, ...) S7_dispatch())
method(gen, BClass) <- function(x, ...) "old"

## -----------------------------------------------------------------------------
# yourPackage, version 1 again
gen := new_generic("x")

# depPackage adds `verbose` ahead of yourPackage's release: no warning under version 1...
method(gen, BClass) <- function(x, verbose = FALSE, ...) "new"

# ...and none under version 2 either
gen := new_generic("x", fun = function(x, verbose = FALSE, ...) S7_dispatch())
method(gen, BClass) <- function(x, verbose = FALSE, ...) "new"

## -----------------------------------------------------------------------------
gen := new_generic("x", fun = function(x, verbose = FALSE, ...) {
  if (!missing(verbose)) {
    .Deprecated(msg = "The `verbose` argument is deprecated and will be removed in a future release.")
  }
  S7_dispatch()
})
method(gen, BClass) <- function(x, verbose = FALSE, ...) "result"
. <- gen(BClass(), verbose = TRUE)

## -----------------------------------------------------------------------------
gen := new_generic("x", fun = function(x, verbose = lifecycle::deprecated(), ...) {
  if (lifecycle::is_present(verbose)) {
    lifecycle::deprecate_warn("1.0.0", "gen(verbose)")
  }
  S7_dispatch()
})
method(gen, BClass) <- function(x, verbose = lifecycle::deprecated(), ...) "result"
. <- gen(BClass(), verbose = TRUE)

## -----------------------------------------------------------------------------
gen := new_generic("x", fun = function(x, drop = TRUE, ...) S7_dispatch())
method(gen, BClass) <- function(x, drop = FALSE, ...) drop
gen(BClass())

## -----------------------------------------------------------------------------
# yourPackage, version 1
gen1 := new_generic("x")

# yourPackage, version 2
gen2 := new_generic("x")
gen1 := deprecated_generic(new = gen2, when = "2.0.0")

## -----------------------------------------------------------------------------
method(gen1, BClass) <- function(x, ...) "result"
gen2(BClass())
gen1(BClass())

## -----------------------------------------------------------------------------
gen1 := deprecated_generic(
  new = gen2, 
  when = "2.0.0", 
  method = "lifecycle(warn)"
)
method(gen1, BClass) <- function(x, ...) "result"
gen1(BClass())

## -----------------------------------------------------------------------------
# yourPackage, version 1
gen := new_generic("x")

# yourPackage, version 2
gen := deprecated_generic("x", when = "2.0.0")
method(gen, BClass) <- function(x, ...) "result"
gen(BClass())

## ----eval = FALSE-------------------------------------------------------------
# # yourPackage, version 2
# #' @importFrom yourPackageCore gen
# #' @export
# yourPackageCore::gen

## ----eval = FALSE-------------------------------------------------------------
# # DO NOT DO THIS
# #' @export
# gen <- yourPackageCore::gen

## ----eval = FALSE-------------------------------------------------------------
# # In yourPackage, version 2
# gen := deprecated_generic(new = yourPackageCore::gen, when = "2.0.0")

## -----------------------------------------------------------------------------
class1 := new_class(package = "foo", properties = list(a = class_any))
class2 := new_class(class1, package = "foo", properties = list(b = class_any))
class2@constructor

## ----eval = FALSE-------------------------------------------------------------
# class2 := new_class(foo::class1, properties = list(b = class_any))
# class2@constructor
# #> function (..., b = NULL)
# #> S7::new_object(S7::as_class(foo::class1)(...), b = b)

## -----------------------------------------------------------------------------
# yourPackage, version 1
validate_count <- function(value) {
  if (any(value < 0)) "must be non-negative"
}
Foo := new_class(properties = list(
  count = new_property(class_double, default = 1, validator = validate_count)
))

# yourPackage, version 2
Foo := new_class(properties = list(
  deprecated_property(
    "count", when = "2.0.0", class = class_double, default = 1,
    validator = validate_count
  )
))

foo <- Foo(count = 3)
foo@count
foo@count <- 4
print(foo)

## -----------------------------------------------------------------------------
Foo := new_class(properties = list(
  size = new_property(class_double, default = 1),
  deprecated_property("count", new = "size", when = "2.0.0")
))

foo <- Foo(size = 3) # silent
foo <- Foo(count = 3) # warns, then sets size
foo@count
foo@count <- 4
foo@size

## -----------------------------------------------------------------------------
# yourPackage, version 1
Foo := new_class(properties = list(size = class_double))

# yourPackage, version 2: warn instead of failing
Foo := new_class(
  properties = list(size = class_double),
  validator = function(self) {
    if (self@size != trunc(self@size)) {
      warning("`size` should be a whole number; this will become an error in a future release.")
    }
    NULL
  }
)
foo <- Foo(size = 3.5)
foo

## -----------------------------------------------------------------------------
# yourPackage, version 1
Foo := new_class(properties = list(size = class_double))
saved <- Foo(size = 1)
MyFoo := new_class(parent = Foo)
measure := new_generic("x")
method(measure, Foo) <- function(x) x@size

# yourPackage, version 2: export both classes
Bar := new_class(properties = list(size = class_double))
Foo := deprecated_class(
  properties = list(size = class_double),
  new = Bar,
  when = "2.0.0"
)
foo <- Foo(size = 2) # warns and still constructs a Foo
measure(foo)
measure(MyFoo(size = 3)) # existing subclasses still work, without warning
measure(saved)
S7_inherits(saved, Foo)
S7_inherits(saved, Bar)

## -----------------------------------------------------------------------------
method(measure, Foo | Bar) <- function(x) x@size
measure(Bar(size = 4))
measure(saved)

## -----------------------------------------------------------------------------
Foo := deprecated_class(new = Bar, when = "2.0.0", alias = TRUE)
foo <- Foo(size = 2) # warns and constructs a Bar
S7_inherits(foo, Bar)

describe := new_generic("x")
method(describe, Foo) <- function(x) paste("Size:", x@size)
describe(Bar(size = 3))

Box := new_class(properties = list(item = Foo))
Box(item = Bar(size = 4))

## -----------------------------------------------------------------------------
S7_inherits(saved, Foo) # FALSE: Foo now refers to Bar

## -----------------------------------------------------------------------------
Retired := deprecated_class(
  properties = list(size = class_double),
  when = "2.0.0"
)
Retired(size = 3)

## ----eval = FALSE-------------------------------------------------------------
# # In yourPackage
# Foo := deprecated_class(
#   properties = list(size = class_double),
#   new = yourPackageCore::Foo,
#   when = "2.0.0"
# )

