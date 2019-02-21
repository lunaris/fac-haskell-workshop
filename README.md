# Founders and Coders Haskell workshop

This repository aims to introduce people to Haskell, a purely functional,
strongly-typed programming language. It includes a number of exercises,
beginning with the basis and working up to a small HTTP server with some JSON
endpoints.

## Tools, prerequisites and assumptions

We recommend you use `stack`, a tool for managing and building projects composed
of one or more Haskell packages, to work through this set of exercises. You can
install `stack` by running the following command at a terminal:

```
$ curl -sSL https://get.haskellstack.org/ | sh
```

Once this has completed, you should be able to see that `stack` has been
installed:

```
$ stack --version
Version X.Y.Z, Git revision SHA ...
```

With that installed, you can build this repository by running `stack build` in
the directory you have checked it out to:

```
$ git clone https://github.com/lunaris/fac-haskell-workshop
...
$ cd fac-haskell-workshop
$ stack build
```

This should download the necessary Haskell compiler and associated toolchain and
also any dependencies that the code in this workshop requires. This can take a
while the first time you run it so it's recommended that you run this before the
workshop if you can. With that done you should be good to go!

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

Haskell has `if`-`then`-`else` too, but since everything is an expression, `if`
returns values and you must always provide an `else` (this is a bit like the
ternary/"Elvis" operator in JavaScript -- `p ? t : f` or `?:`):

```
*Main> if True then 'a' else 'b'
'a'
*Main> if 42 > 3 then 3 * 4 else 4 * 5
12
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
('c',False)
*Main> :t ('c', False)
('c',False) :: (Char, Bool)
*Main> ('c', 'd', 'e')
('c','d','e')
*Main> :t ('c', 'd', 'e')
('c','d','e') :: (Char, Char, Char)
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
[False,True]
*Main> :t [False, True]
[False,True] :: [Bool]
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

### Writing some code: simple functions

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

**Exercise**: Write a function `square` which takes an `Int` and returns its
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

**Exercise**: Write a function `quadratic` which computes the result of quadratic
polynomials of the form `ax^2 + bx + c`. Your function should take `Int`s
representing `a`, `b`, `c` and `x` and return the result (also an `Int`). Feel
free to use the `square` function in your implementation if you wish.

```
*Main> quadratic 3 4 5 6
137
*Main> quadratic 3 2 5 3
38
```

**Exercise**: Write a function `factorial` which takes an `Int` and computes its
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

If, when writing `factorial`, you _are_ worried about negative inputs, you can
use _guards_ to defend your function clauses against arbitrary predicates. For
instance, here's a function that doubles positive numbers and returns zero in
all other cases:

```
doublePositive :: Int -> Int
doublePositive x
  | x > 0     = 2 * x
  | otherwise = 0
```

Each guard is defined using the `|` symbol and some boolean. If that boolean
evaluates to `True`, that guard's body will be evaluated. If it's `False`, the
next guard or clause will be tried. `otherwise` isn't a special keyword but just
a nicety for `True`:

```
otherwise :: Bool
otherwise = True
```

While they behave similarly to `if`-`then`-`else` expressions, guards have a
slightly different purpose (selecting whether or not a clause should be
evaluated).

---

**Exercise**: Using guards, write a function `doubleEven` that doubles
event `Int`s and returns odd `Int`s unchanged.

```
*Main> doubleEven 3
3
*Main> doubleEven 14
28
```

**Exercise**: Rewrite `doubleEven` using an `if`-`then`-`else` expression and
compare the two approaches.

**Exercise**: Using guards, write an extended version of `doubleEven`,
`weirdMaths`, that implements the following rules:

* Even numbers are doubled
* Negative odd numbers are made positive
* Positive odd numbers are incremented to the next even number.

```
*Main> weirdMaths 42
84
*Main> weirdMaths (-3)
3
*Main> weirdMaths 0
0
*Main> weirdMaths 39
40
```

You may wish to implement this with `if` too and (hopefully) observe that guards
offer a slightly cleaner expression.

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

**Exercise**: `(==)` and `(/=)` are also polymorphic -- observe that you can do
both `False == True` and `'c' == 'd'` with the same function, for instance. What
are the types of `(==)` and `(/=)`? What is the name of the constraint required
to use them?

**Exercise**: Generalise the types of your `factorial` and `quadratic` functions
from earlier. Note that for `factorial` you may need more than one constraint --
if you get stuck remember that GHCi can help you find the type!

---

### Kicking pattern matching up a notch

While we've introduced tuples and lists, we've not seen how to write functions
that consume and produce them yet. Let's start with tuples. Here's a function
that takes a pair of an `Int` and a `Bool` and returns it unmodified:

```
doNothingPair :: (Int, Bool) -> (Int, Bool)
doNothingPair p = p
```

Observe that we can bind a name (here `p`) to a tuple argument just as we could
an `Int`, `Char` etc. At the type level, you might notice that this function
does not require that the pair given consists of an `Int` and a `Bool` -- all
that matters is that the resulting pair has the same type as the input. We can
generalise the type thus:

```
doNothingPair :: (a, b) -> (a, b)
doNothingPair p = p
```

However, suppose we _do_ want to take a pair where the first element really is
an `Int`, and we want to increment that `Int`. How do we do it? Well, we can
pattern match _into_ the tuple:

```
incrementFst :: (Int, b) -> (Int, b)
incrementFst (x, y) = (x + 1, y)
```

To the left of the `=` sign, the _pattern_ `(x, y)` says "match this function's
argument to a pair of two elements, which we'll call `x` and `y`". To the right
is the function body, in which we _build a new tuple_ whose elements are `x + 1`
and `y` unchanged. The _type_ of the function reflects this -- the first element
must be an `Int`, but the second could be anything so long as that type is
preserved:

```
*Main> incrementFst (10, False)
(11,False)
*Main> incrementFst (0, 'c')
(1,'c')
```

---

**Exercise**: As you now know, it is not only `Int`s that can be incremented.
Generalise the type of `incrementFst` so that it works on pairs whose first
element is of _any numeric type_.

**Exercise**: `fst` and `snd` only work on pairs. Write functions `fst3`, `snd3`
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

Pattern matching on lists is a bit more involved, since while (for example) a
pair "always looks like a pair", a list might look like many things. It could be
empty (`[]`), it could have one element (`[1]`), it could have four elements
(`"abcd"`), and so on. In reality, Haskell only makes one distinction -- a list
is either _empty_ or it's an element glued on to the front of another
(potentially empty) list. The definition of the list type says exactly this
(albeit in a funny syntax we'll elaborate on below):

```
data [a]
  = []
  | a : [a]
```

So, a list of values of type `a` (since lists can hold lots of types of values)
is either empty (represented by the _constructor_ `[]`) or an element of type
`a` joined to another list of `a`s by the _constructor_ `(:)` (which is
pronounced "cons", short for list constructor). Up until now we've used some
syntactic sugar to write lists, but it's time to lift the curtain:

```
*Main> 1 : 2 : 3 : []
[1,2,3]
*Main> []
[]
*Main> 'a' : 'b' : []
"ab"
*Main> :t False : True : []
False : True : [] :: [Bool]
```

All lists are made from the empty list (`[]`, sometimes also called "nil") and
"consing" on elements with `(:)`. Consequently, _all lists can be pattern
matched with the same two things_. Let's see how `length` is implemented:

```
length :: [a] -> Int
length [] =
  0
length (x : xs) =
  1 + length xs
```

You can read the first clause as saying "the length of an empty list is zero".
The second clause should be read a "the length of a non-empty list, where the
first element (or _head_) is called `x` and the remaining elements (or _tail_)
are called `xs`, is one plus the length of `xs`". Note that `x` is ignored --
this is totally fine, but if you want you can make this sort of thing explicit
by using the "wildcard" pattern:

```
length (_ : xs) =
  1 + length xs
```

Here, we've chosen not to name the head of the list to emphasize that its value
is not important in computing the length.

We can produce lists in the same manner too -- suppose we want to double every
`Int` in a list:

```
doubleAll :: [Int] -> [Int]
doubleAll [] =
  []
doubleAll (x : xs) =
  (2 * x) : doubleAll xs
```

---

**Exercise**: Write a function `null`, which takes a list of values of _any type_
and returns `True` if and only if the list is empty. As with `length`, there
will be two cases, but it needn't be a recursive function.

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

**Exercise**: Write a function `tail`, which takes a list of values of _any type_
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

**Exercise**: Write a function `sum`, which takes a list of `Int`s and adds them
up.

```
*Main> sum []
0
*Main> sum [1..10]
55
*Main> sum [-1,1]
0
```

**Exercise**: Generalise `sum`'s type so that it works on lists of values of _any
numeric type_.

**Exercise**: Write a function `fsts`, which takes a list of _pairs_ of any
types and returns a list of the first elements.

```
*Main> fsts [('a',1),('b',2),('c',3),('d',4)]
"abcd"
```

---

### Higher-order functions and more data types

Recall that earlier we finally saw how the list data type is _defined_:

```
data [a]
  = []
  | a : [a]
```

Other types can be defined in the same manner. For instance values of type
`Maybe a` _might_ be present (in which we find `Just` some value), or might be
absent (in which case find `Nothing`):

```
data Maybe a
  = Nothing
  | Just a
```

So valid values of type `Maybe Bool` are `Nothing`, `Just False` and `Just
True`. `Just 'c'` is a `Maybe Char`, `Nothing` is a `Maybe a` (for _any_ `a`)
and `Just "hello"` is a `Maybe String`. Pattern matching and construction work
as they do for lists; here's a function that adds three to an `Int` if it's
`Just` there:

```
maybeAddThree :: Maybe Int -> Maybe Int
maybeAddThree Nothing =
  Nothing
maybeAddThree (Just x) =
  Just (x + 3)
```

---

**Exercise**: Write a function `safeHead` that takes a list of any type and
returns `Just` the head if it exists and `Nothing` if the list is empty.

**Exercise**: Write a function `maybeLength` that accepts a `Maybe String` and
returns the length of that string if it's present and `0` if it's not.

**Exercise**: Write a function `lookup` that takes a _key_ of some type `a` and
a list of _key-value pairs_ and returns `Just` the first value whose key matches
that given if it exists and `Nothing` otherwise. Note that the type `a` will
need a constraint, since you'll need to be able to check whether the key you're
given is _equal_ to each key in the list. Note that both guards and
`if`-`then`-`else` will work fine for comparing each key.

---

If you haven't tried/stumbled upon it already, observe that `Just not` is a
perfectly fine value. You can't print it (functions can't generally be printed
in Haskell), but it has a valid type:

```
*Main> :t Just not
Just not :: Maybe (Bool -> Bool)
```

Indeed, functions are _entirely first-class_ in Haskell -- pass them as
arguments, stick them in lists; anything you can do with an `Int` you can
probably do with a function. This turns out to be _immensely_ powerful. Recall
the `doubleAll` function from earlier:

```
doubleAll :: [Int] -> [Int]
doubleAll [] =
  []
doubleAll (x : xs) =
  (2 * x) : doubleAll xs
```

We can generalise this so that it takes _any function that takes an `Int` and
returns an `Int`_ and applies that instead:

```
mapInts :: (Int -> Int) -> [Int] -> [Int]
mapInts f [] =
  []
mapInts f (x : xs) =
  f x : mapInts f xs
```

In fact, we can go further and produce `map`, which works like `map` in
JavaScript:

```
map :: (a -> b) -> [a] -> [b]
map f [] =
  []
map f (x : xs) =
  f x : map f xs
```

`map` takes a function that turns `a`s into `b`s, along with a list of `a`s and
gives you a list of `b`s by applying that function to every element. Thus
`doubleAll` could become:

```
doubleAll :: [Int] -> [Int]
doubleAll xs = map (2 *) xs
```

Or even, since functions are first-class (!):

```
doubleAll :: [Int] -> [Int]
doubleAll = map (2 *)
```

---

**Exercise**: Write a function `filter`, which takes a predicate over some type
and a list of values of that type and returns the list of elements that satisfy
that predicate.

```
*Main> filter even [1..10]
[2,4,6,8,10]
*Main> filter (> 4) [3,5,9,1,4,2,6]
[5,9,6]
```

**Exercise**: Write a function `find`, which takes a predicate over some type
and a list of values of that type and returns `Just` the first element that
satisfies the predicate or `Nothing` if no element satisfies it.

```
*Main> find even [1,2,3,4]
Just 2
*Main> find odd [2,4..10]
Nothing
*Main> find (> 2) []
Nothing
```

---

### Something more real-world

OK, this is kind of cool, but can you actually _do_ anything with it? Yes,
definitely, but given the scope of this workshop it's going to get a bit
hand-wavey (though you are free to make a more rigourous journey independently).
In this section we'll spin up a simple HTTP API/web server, using a Haskell
library called `scotty` which behaves a bit like Express (JavaScript) or Sinatra
(Ruby), if you've ever played with those.

Haskell programs begin from a function called `main`. Here we've set up `main`
to create a little web server listening on port 6060:

```
scotty 6060 $ do
  ...
```

The `do`-block is a subset of Haskell that allows you to write code in an
"imperative" fashion, as you might in other languages. Under the hood, `do` is
actually just using functions (like everything else in Haskell!), but we'll
gloss over that here and just use it to `do` (sorry) stuff.

So, in the `do`-block for the web server we've started, we define some routes:

```
get "/greetings/:name" $ do
  name <- param "name"
  html $ mconcat ["<h1>Hello, ", name, "!</h1>"]
```

This is pretty close to e.g. what you'd write in Express:

```
app.get("/greetings/:name", function (req, res) {
  var name = req.params.name
  res.send("<h1>Hello, " + name + "!</h1>");
});
```

You can run the `main` function just like any function in GHCi (though it'll run
"forever" until you abort it with Control-C, since it's a server):

```
*Main> main
Setting phasers to stun... (port 6060) (ctrl-c to quit)
```

If you open
[http://localhost:6060/greetings/fac](http://localhost:6060/greetings/fac) in a
web browser you'll see the `Hello, fac!" header printed before you. Nice!

---

**Exercise**: Add a `GET` endpoint, `/formal-greetings/...`, that takes two
parameters, a forename and a surname (say) and produces some HTML greeting the
user formally, e.g.:

```
$ curl http://localhost:6060/formal-greetings/Joe/Bloggs
<h1>Good day, Joe Bloggs!</h1>
```

It may help to know that `mconcat` will concatenate an arbitrary list of textual
values.

**Exercise**: As in Express, there are other HTTP verbs you can play with too.
Write a `DELETE` endpoint, `/users/...` that takes a username and confirms that
it has been deleted, e.g.:

```
$ curl -XDELETE http://localhost:6060/users/root
User "root" has been deleted
```

---

It's pretty difficult to build an HTTP API without talking about JSON these
days, so we'll cover that next. First up, let's look at _records_, which are
Haskell types with named fields:

```
data Person = MkPerson
  { name :: String
  , age  :: Int
  }
```

`Person` is a record type which can be built by giving the _constructor_
`MkPerson` two fields -- a `name`, of type `String`, and an `age`, of type
`Int`:

```
*Main> MkPerson { name = "Joe", age = 30 }
MkPerson {name = "Joe", age = 30}
```

Technically you don't need to give the field names (`MkPerson "Joe" 30` would
work just as well) but they often help readability. Note also that types and
values have different namespaces in Haskell, so you will often see something
like:

```
data Person = Person
  { name :: String
  , age  :: Int
  }
```

in which the _type_ `Person` has a _constructor_ also named `Person`. This is
fine, albeit a little confusing at first, especially if you ask GHCi what the
type of `Person` (formerly `MkPerson`) is:

```
*Main> :t Person
Person :: String -> Int -> Person
```

That is, `Person` (the constructor), takes a `String` (the `name`) and an `Int`
(the `age`) and gives you a `Person` (the record type we defined). We can see
that the definition of the `Person` record type isn't that far removed from a
JSON object, and that if we wanted to represent a `Person` as JSON there would
be a fairly natural choice:

```
{
  "name": "Joe",
  "age": 30
}
```

This goes for other records too -- suppose we define a record to represent
programming languages:

```
data Language = Language
  { name        :: String
  , description :: String
  }
```

We might build values like:

```
*Main> Language { name = "Haskell", description = "Awesome!" }
Language {name = "Haskell", description = "Awesome!"}
*Main> Language { name = "JavaScript", description = "Not too shabby!" }
Language {name = "JavaScript", description = "Not too shabby!"}
```

and expect JSON like:

```
{ "name": "Haskell", "description": "Awesome!" }
{ "name": "JavaScript", "description": "Not too shabby!" }
```

It seems therefore that serialising to and deserialising from JSON is a
capability we might apply to multiple types. As with numbers and equality, the
solution Scotty uses in this case is a pair of constraints, `ToJSON` and
`FromJSON`, along with functions like:

```
json :: ToJSON a => a -> ...
```

which takes a value of _any type whose values can be serialised to JSON_ and
sends that value as the response to a request. So, in Express, you might do:

```
res.send({
  "name": "Haskell",
  "description": "Awesome!"
});
```

With the `json` function, Scotty allows you to do something similar:

```
json (Language {
  name = "Haskell",
  description = "Awesome!"
})
```

One gotcha is that, in order to make this work, we have to tell Haskell that
e.g. `Language` and `Person` are types that satisfy the `ToJSON` and `FromJSON`
constraints (alternatively, are members of the `ToJSON` and `FromJSON` _type
classes_). Fortunately, there are a bunch of mechanisms in Haskell that can be
combined to make GHC write this code itself, through a so-called `deriving`
clause:

```
data Language = Language
  { name        :: Name
  , description :: Description
  }
  deriving (Eq, Ae.FromJSON, Generic, Show, Ae.ToJSON)
```

This takes care of the issue, and also makes sure that we can compare
`Language`s (through `Eq`) and print them out in GHCi (through `Show`). The
ability to have GHC generate all kinds of boring "boilerplate" code like this is
one of Haskell's killer features and can be seen as some of the "reward" for
taking the effort to spell out the types.
