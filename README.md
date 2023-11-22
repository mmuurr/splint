Where a cast is strong, a splint is weak -- but splinting is often more useful in the wild.


# Splint

## Introduction

The {splint} package is like a first-aid kit for R 'typed' programming.
It provides some assistance to keep things aligned and safe -- but at its core R is dynamically-typed and a splint can only do so much.
As with all first-aid tools, you must apply the splint correctly to get any protection & value from it 😉.


At the simplest level, a `splint` is a function that takes at least one argument: an object on which to apply the splint.
This function can perform any transformation you like, but in spirit should be a casting-like operation.
Common examples include `as.character()` or `as.double()`.

Splinting is like casting, but a splint is:
* more lightweight than a cast,
* weaker than a cast and can sometimes fail,
* easier to improvise than a cast,
* formalizable into a 'type' (and in some cases a 'prototype').


* and because of these properties, _sometimes_ more useful than a cast.

At present, splints should produce exactly one of two type of outputs: **vector**-ish or **dictionary**-ish.
(_-ish_ because in R nearly all objects, including dictionary-like lists, are considered vectors.)
For the purposes of splinting, these output types have the following properties:
* Vectors (`vctr`)
  * Variable-length.
  * Can naturally be included as a column in a data frame.
  * Examples include character vectors and unnamed lists. (Technically such a list can include names, too, but the implication here is that the list is array-like, as opposed to dictionary-like.)
* Dictionaries (`dict`)
  * Typcially implemented as named lists; each name is also called a "key".
  * Alternative implementations, e.g. an R6 class that 'looks-like' a list may also be possible.
  * Keys should be unique.
  * Do not naturally fit the concept of being included as a column _as-is_ in a data frame (despite being a list).
    Rather, a list-col _of dictionaries_ would be the typical implementation, e.g. a list-col of name dicts might look like:
	```R
	[ { first:"John", last:"Doe" }, { first:"Jane", last:"Doe" }, ... ]
	```






All splint functions:
1. have signature `function(x, ...)`, where `x` is the object to be splinted.
2. have class `"splint.splint"`.

Most splints also have some attributes, the details of which will be discussed later:
* `f`: the underlying function used by a _simple_ splint.
* `missing`: this is an instruction for how a _container_ splint should deal with this splint when data is missing.
* `splints` this is list of splints passed to a _container_ splint.

## Simple Splint

The simplest of all splints is the `simple_splint`, _constructed_ via the function `splint_simple`.
Typically all splints should be built as closures, i.e. wrappers around the underlying casting/coersion operations.

The simplest of all simple splints wraps an identity function, i.e. a splint that simply returns the object.
Note that {splint}'s concept of `identity()` allows for missing `x` (in which case `NULL` is returned), wherease `base::identity()` errs for missing `x`.
```r
my_splint <- splint_simple()
identical(my_splint(1), splint:::identity(1))
identical(my_splint(NULL), splint:::identity(NULL))
identical(my_splint(NULL), splint:::identity())
identical(my_splint(), NULL)
```



