Where a cast is strong, a splint is weak; but splinting is often more useful in the wild.


# Splint

## Introduction

The {splint} package is like a first-aid kit for R 'typed' programming.
It provides some assistance to keep things aligned and safe -- but at its core R is dynamically-typed and a splint can only do so much.
As with all first-aid tools, you must apply the splint correctly to get any protection & value from it 😉.

At the simplest level, a `splint` is a function that takes at least one argument: an object on which to apply the splint.
This function can perform any transformation you like, but in spirit should be a casting-like operation.
Common examples include `as.character()` or `as.double()`.

## API

### Simple splint

Simple splints typically are used for atomic vector types and are the 'building blocks' of more sophisticated splints (like dictionaries and tables).

```r
splint_simple <-
  function(f = identity, klass = character(0))
```

`f` should be a one-arg function.
When splinting this splint simply delegates to `f` and prepends `klass` to the class of the returned value.

#### Examples:

```r
splint_simple(as.character)
splint_simple(as.integer, "my.cool.integer.type")
splint_simple(as.logical, c("klass.can", "be.multiple", "types"))
```

### Dict(ionary) splint

A dict(ionary) splint will always coerce to a dict (uniquely-named list).
Each element of the dict splint must be a splint; this is best demonstrated via examples.

#### Examples:

First let's create a simple "address" dict splint:
```r
as_address <-
  splint_dict(list(
	street_number = splint_simple(as.integer),  ## let's ignore those weird "1/2" addresses
	street_name = splint_simple(as.character),
	unit_number = splint_simple(as.integer),
	postal_code = splint_simple(as.character),
	city = splint_simple(as.character),
	state = splint_simple(as.character)
  ),
  klass = "address")

as_address(dict(
  street_number = 10,
  street_name = "Elm St",
  unit_number = 4,
  postal_code = 12345,  ## will be splinted to chr
  city = "Springfield",
  state = "Massachusetts"
)) |> str()
```

Let's add some restrictions to soft-force each field to be a scalar:
```r
as_scalar_int <- function(x) as.integer(x)[1]
as_scalar_chr <- function(x) as.character(x)[1]
as_address <- splint_dict(list(
  street_number = splint_simple(as_scalar_int),
  street_name = splint_simple(as_scalar_chr),
  unit_number = splint_simple(as_scalar_int),
  postal_code = splint_simple(as_scalar_chr),
  city = splint_simple(as_scalar_chr),
  state = splint_simple(as_scalar_chr)
), klass = "address")

as_address(dict(
  street_number = c(10, 20),  ## will be (silently) trimmed
  street_name = "Elm St",
  unit_number = 4,
  postal_code = c(12345, 23456),  ## will be (silently) trimmed
  city = "Springfield",
  state = "Massachusetts"
)) |> str()
```

We can nest other dicts:
```r
as_person <- splint_dict(list(
  first_name = splint_simple(as_scalar_chr),
  last_name = splint_simple(as_scalar_chr),
  age = splint_simple(as_scalar_int),
  dog = splint_dict(list(
    name = splint_simple(as_scalar_chr),
	breed = splint_simple(as_scalar_chr)
  ), klass = "dog")
), klass = "person")
```

as_string <- function(x) as.character(x) |> checkmate::assert_string()
as_date
as_person <- splint_dict(
  klass = "person",
  splints = dict(
    first_name = splint_simple(as_string),
	last_name = splint_simple(as_string),
	birth_date = splint_simple(\(x) lubridate::as_date(x)[1]),
	mother = splint_splint("person"),
	father = splint_splint("person")
  )
)

as_person <- splint_dict(dict(
  first_name = splint_simple(as_scalar_chr),
  last_name = splint_simple(as_scalar_chr),
  mother = splint_recurse("person"),
  father = splint_recurse("person")
), klass = "person", id = "person")



### "Missing" sentinel

All splints are coercion functions with some common properties:
1. The splint will apply a `klass` after coercion duties -- prepending `klass` to the `class` attribute of the returned val.
2. If a special "missing" sentinel value is passed, then behavior is dictated by that sentinel's rules (described below).









## WIP

### Classname Scope

In JS, you can do this:
```
class Node {
    constructor(value, next = null) {
        this.value = value;
        this.next = next;
    }

    addNext(value) {
        this.next = new Node(value);  // Node is in scope
    }
}
```

And with class expressions that aren't hoisted, this:
```
const Node = class NamedNode {
    constructor(value, next = null) {
        this.value = value;
        this.next = next;
    }

    addNext(value) {
        this.next = new NamedNode(value); // Referencing NamedNode within its body
    }
};
```

In that second example, `NamedNode` is availale within its body.
This is what we'd like to replicate in our R system for supporting recursive definitions.


### Other



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




