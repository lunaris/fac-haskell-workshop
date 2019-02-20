# Founders and Coders Haskell workshop

This repository aims to introduce people to Haskell, a purely functional,
strongly-typed programming language. It includes a number of exercises,
beginning with the basis and working up to a small HTTP server with some JSON
endpoints.

## Tools, prerequisites and assumptions

* `stack`, a tool for managing and building projects composed of one or more
  Haskell packages.

## Exercises

### Getting comfortable with the REPL

A _read-eval-print-loop_ (REPL) is an interactive environment in which programs
can be developed and tested. Users of a REPL can type expressions at a prompt,
which are _read_ and _evaluated_ before a result is _printed_ back. If you've
played with the JavaScript console in e.g. Chrome or Firefox, this is an example
of a REPL. In this workshop, we'll use GHCi, a REPL for Haskell code that
forms part of the _Glasgow Haskell Compiler_ (GHC), arguably the most prominent
toolchain for working with Haskell programs. You can load a GHCi REPL by
running:

```
$ stack repl
```

Next, we'll enter some expressions. Let's start with some arithmetic:

```
*Main> 2
2
*Main> 2 + 5
7
```

You can confirm that all your favourite operators are here and that precedence
(order of evaluation) works as you expect:

```
*Main> 3 * 4
12
*Main> 4 * 5 + 3
23
*Main> 4 * (5 + 3)
32
```

Like JavaScript, Haskell has _booleans_ (`True` and `False`) that can be worked
with directly or produced as the result of e.g. comparison and logical operations:

```
*Main> False
False
*Main> 3 > 1
True
*Main> True && False
False
```

Plenty of other functions produce booleans, too -- the `even` function returns
`True` if and only if its argument is an even number, while the `not` function
returns the logical inverse of the boolean it is given:

```
*Main> even 2
True
*Main> even 3
False
*Main> not False
True
*Main> not (even 3)
True
```

Note that function application is achieved by _juxtaposition_ -- the expression
`f x` applies the function `f` to the argument `x`. There is no need for
parentheses as in e.g. JavaScript (e.g. `f(x)`). Note that writing e.g. `not
(False)` _will_ actually work, but this is a coincidence that does not
generalise -- more on this later.

Characters and strings are another language staple you can find in Haskell,
though note that unlike JavaScript, Haskell has separate types for characters
and strings. The former are enclosed in _single quotes_ and consist of _exactly
one_ character; the latter are enclosed in _double quotes_ and can, naturally,
contain zero or many characters:

```
*Main> 'c'
'c'
*Main> "hello"
"hello"
```

When we talk about "separate types", this is a distinction GHCi is itself
capable of (and insistent upon) making -- you can use the `:t` command (short
for `:type`, which will also work), to reveal the type of any expression:

```
*Main> :t 'c'
'c' :: Char
*Main> :t not (even 2)
not (even 3) :: Bool
```

You can read the double colon, `::`, as "has the type". So the character `'c'`
"has the type" `Char`; the expression `not (even 3)` "has the type" `Bool`
(which is the name of the type of booleans in Haskell). Note that functions like
`not` and `even` have types too:

```
*Main> :t not
not :: Bool -> Bool
```

You can read this as "`not` takes a `Bool` and returns a `Bool`".

Before we finish the introduction, there are a couple of other types you should
have in your Haskell toolbox. First up are _tuples_, which are just collections
of values that don't have to have the same type, like pairs, triples, etc. The
tuple `('c', False)` is a pair of a `Char` and a `Bool` and has the type `(Char,
Bool)`; the tuple `('c', 'd', 'e')` is a triple of three `Char`s and has the
type `(Char, Char, Char)`:

```
*Main> ('c', False)
('c', False)
*Main> :t ('c', False)
('c', False) :: (Char, Bool)
*Main> ('c', 'd', 'e')
('c', 'd', 'e')
*Main> :t ('c', 'd', 'e')
('c', 'd', 'e') :: (Char, Char, Char)
```

1-tuples are just ordinary values (`('c')` is simply the character `'c'`). This
ties into the point about function application earlier -- do _not_ add
parentheses unless you are either indicating precedence (e.g. `even (2 + 3)`) or
passing a function _a tuple argument_. That is, a call `f ('c', False)` does not
indicate a function `f` that takes two arguments (the first a `Char`, the second
a `Bool`) as it would in JavaScript. It indicates a function that takes _a
single argument that is a pair of a `Char` and a `Bool`_. A two-argument
function as described would simple use more juxtaposition, as in `f 'c' False`.
Two functions that take tuples are `fst` and `snd`, which return the first and
second element of a pair, respectively:

```
*Main> fst ('c', False)
'c'
*Main> snd (True, 'b')
'b'
```

The last type we'll cover in this section is that of _lists_. Like tuples, lists
are collections of values. However, unlike tuples, all the values in the list
must have the _same type_. Thus we may have a _list of booleans_, written `[False,
True]` which has the type `[Bool]`, or a list of characters, written `['a', 'b',
'c']` or `"abc"` (strings are just lists of characters) which has the type
`[Char]` (or `String`):

```
*Main> [False, True]
[False, True]
*Main> :t [False, True]
[False, True] :: [Bool]
*Main> ['a', 'b', 'c']
"abc"
```

Many types can be enumerated and this can be used as a shorthand when writing
lists:

```
*Main> ['a'..'z']
"abcdefghijklmnopqrstuvwxyz"
*Main> [1..10]
[1,2,3,4,5,6,7,8,9,10]
```

### Writing some code: functions

We saw earlier that `not :: Bool -> Bool`: `not` is a function that takes a
`Bool` and returns a `Bool`. If you load up the `Main.hs` file in this
repository, you can see the definition of `not` near the top of the file:

```
not :: Bool -> Bool
not False = True
not True  = False
```

The first line states that `not` has the aforementioned type. The second two
are clauses defining how the function behaves. Haskell supports _pattern
matching_, where a clause can demand that its input has a certain shape. Here,
each clause demands that its input is a specific boolean -- in the first clause,
the boolean `False`; in the second, the boolean `True`. In each case then the
only job left is to return the logical opposite of the matched argument --
`True` in the case of receiving `False` and `False` in the other case. You might
think of pattern matching as similar to equality checks in languages like
JavaScript, viz.:

```
function not(x) {
  if (x === false) {
    return true;
  } else if (x === true) {
    return false;
  }
}
```

This is not a bad approximation but the reality is that pattern matching is far
more powerful, as we shall see. That said, this comparison serves to highlight a
couple of other features and differences present in the Haskell program:

* Since everything is implicitly an expression, we do not need to use a keyword
  like `return` to signal what value is being produced by the function.

* Like the `if`/`else if`, pattern matches are tried in the order they are
  defined, top to bottom, and are exclusive -- if the first clause matches, the
  second will never be executed, and so on.

As well as specifying certain values, such as `False` or `True`, patterns can
_bind_ variables. For instance, here's a function that takes an integer and
returns ten times the value given:

```
timesTen :: Int -> Int
timesTen x = 10 * x
```

Here, we don't match a specific integer or set of integers, we simply bind the
name `x` to the argument. This is entirely similar to writing the following
JavaScript:

```
function timesTen(x) {
  return 10 * x;
}
```

or, perhaps in more modern parlance:

```
const timesTen = (x) => 10 * x;
```

Note that, just as in other languages, the body of a function is free to make
use of another function (here the `(*)` operator).

---

*Exercise*: Write a function `square` which takes an `Int` and returns its
square (another `Int`).

```
*Main> square 4
16
*Main> square 14
196
```

Note that when you save changes to your code, you will have to tell GHCi to
_reload_ it. You can do this using the `:r` (or `:reload` in full) command:

```
*Main> :r
Ok, one module loaded.
```

If there are errors in your code, GHCi will let you know and refuse to reload
the module until you fix them.

*Exercise*: Write a function `quadratic` which computes the result of quadratic
polynomials of the form `ax^2 + bx + c`. Your function should take `Int`s
representing `a`, `b`, `c` and `x` and return the result (also an `Int`). Feel
free to use the `square` function in your implementation if you wish.

```
*Main> quadratic 3 4 5 6
137
*Main> quadratic 3 2 5 3
38
```

*Exercise*: Write a function `factorial` which takes an `Int` and computes its
factorial (another `Int`). The factorial of a number `N` is defined
_recursively_ thus:

* The factorial of `0` is `1`.
* The factorial of a number `N` is `N` multiplied by the factorial of `N - 1`.

Thus `factorial n` == `n * (n - 1) * (n - 2) * ... * 1` -- `factorial 3 == 6`,
`factorial 5 == 120` and so on. You don't have to worry about negative inputs.

```
*Main> factorial 3
6
*Main> factorial 5
120
*Main> factorial 0
1
```

---

Unlike JavaScript, Haskell distinguishes between whole numbers (3, 6,
242852592295, etc.) and _floating point_ numbers (e.g. 3.14159, 0.1337, etc.).
You've dealt with `Int`s, which is one type for working with the former, but
Haskell has types like `Float` and `Double` too, which work with the latter. For
instance, you might write a variant of your `square` function, `squareFloat`,
that operates on `Float`s instead of `Int`s:

```
squareFloat :: Float -> Float
squareFloat x = x * x
```

This works as we expect:

```
*Main> squareFloat 3.0
9.0
*Main> squareFloat 4.5
20.25
```

Note however that we can't use `square` to square a `Float`, even those `square`
and `squareFloat` have identical bodies:

```
*Main> square 4.5

<interactive>:13:8: error:
    * No instance for (Fractional Int) arising from the literal `4.5'
    * In the first argument of `square', namely `4.5'
      In the expression: square 4.5
      In an equation for `it': it = square 4.5
```

GHCi complains that `square` is only defined on `Int`s but that a fractional
value (the literal `4.5`) has been passed to it instead -- a type error. We
already have a work-around of course---the `squareFloat` function---but it
seems a shame that we can't use a single function with both types.

Indeed, this dramatic build-up has a satisfying conclusion -- Haskell supports
_ad-hoc polymorphism_, in which a single function can be used with many types.
To see how, remove the type signature from your definition of `square`:

```
square x = x * x
```

This will still compile, but GHC will _infer_ the type for you. Brilliantly, it
will infer the most general type, which you can see by reloading and asking
GHCi:

```
*Main> :r
*Main> :t square
square :: Num a => a -> a
```

Observe that the type has been changed in two ways:

* The type `Int -> Int` has changed to become `a -> a`
* There is a new component, `Num a =>`

The new type `a` is a _type variable_. Like a variable in your code, it can
represent a number of things (here types). So you can read the type `a -> a` as
"takes some type, call it `a`, and returns a value of the same type".

The prefix `Num a =>` is called a _constraint_. It says that "while `a` is 'some
type', it can't be _any_ type -- it has to be a type that _satisfies the `Num`
constraint_". The `Num` constraint says that values of type `a` can be treated
"like numbers" e.g. having additions like `+`, `*` and `-`. Indeed, if you ask
GHCi what the type of the `+` function is (you'll need to wrap it in parentheses
so that it's not treated as an infix operator):

```
*Main> :t (+)
(+) :: Num a => a -> a -> a
```

So `(+)` "takes a value of some type `a` (which satisfies `Num`) and another
value of the same type, and returns a value of that type (presumably the sum of
the first two values)".

Type variables like `a` don't _have_ to be constrained -- there are functions
that really do work _for any type_ `a`. Take the humble identity function, which
just returns its argument:

```
id :: a -> a
id x = x
```

`id` works on values of _any_ type -- numbers, characters, booleans, you name
it! Another very general function you've already seen is `length`, which returns
the length of a list. Check out its type:

```
length :: [a] -> Int
```

This says that `length` "takes a list of values of _any type_, `a`, and returns
an `Int`".

---

*Exercise*: `(==)` and `(/=)` are also polymorphic -- observe that you can do
both `False == True` and `'c' == 'd'` with the same function, for instance. What
are the types of `(==)` and `(/=)`? What is the name of the constraint required
to use them?

*Exercise*: `fst` and `snd` only work on pairs. Write functions `fst3`, `snd3`
that retrieve the first and second elements of a triple, and functions `thd4`
and `fth4` that retrieve the third and fourth elements of a 4-tuple. All these
functions should be polymorphic and work no matter what the types of the tuple
elements.

```
*Main> fst3 ('a', False, "hello")
'a'
*Main> snd3 (False, True, 3)
True
*Main> thd4 ('a', "b", False, True)
False
*Main> fth4 (10, "twenty", True, 'z')
'z'
```

---



---

*Exercise*: Write a function `null`, which takes a list of values of _any type_
and returns `True` if and only if the list is empty.

```
*Main> null []
True
*Main> null [1,2,3]
False
*Main> null "abc"
False
*Main> null ""
True
```

*Exercise*: Write a function `tail`, which takes a list of values of _any type_
and returns the list without its first element if it is non-empty and the empty
list if it is empty.

```
*Main> tail "hello"
"ello"
*Main> tail []
[]
*Main> tail [1..10]
[2,3,4,5,6,7,8,9,10]
```

---



---

*Exercise*: Write a function `filter`, which takes a predicate over some type
and a list of values of that type and returns the list of elements that satisfy
that predicate.

```
*Main> filter even [1..10]
[2,4,6,8,10]
*Main> filter (> 4) [3,5,9,1,4,2,6]
[5,9,6]
```

---
