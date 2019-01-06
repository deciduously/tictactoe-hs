# Some Haskell, English'd

A TicTacTour Without Honor or Humanity

## The Intro

Haskell is just not that bad.  What can be scary is how different it can be to work with than what you're used to so you hit a lot more walls at the very beginning.  This post is a deep dive into a program that would be trivial to write in something imperative with the aim of demystifying how you'd go about framing the problem in an unfamiliar paradigm.

As opposed to a traditional tutorial where we build the program step by step this is a top-down, let's see what's here sort of deal. I’m going to start with `main`, the first thing run when you execute the program, and will step through every line of code as it's called and explain what's going on.  Thus, this is more an exercise in reading Haskell than writing it but the two skills are not unrelated and hopefully this can help demystify how to do some simple tasks of your own as well as feel more equipped to approach larger Haskell examples.

### You

Haskell-curious.  This is written with a baseline understanding of programming in an imperative language assumed, but zero Haskell or functional programming knowledge.  If you think you could implement this program in your language of choice, you're good to go.  I'm going to cover why things are the way they are as thoroughly as I can.  If you're brand new to programming and the below program *doesn't* look like something you know how to make, you still should be able to follow me through this, albeit with more effort!  There might be a little extra research on your part required here and there.   If you're not new to functional programming but are to Haskell, there may be a few headers you can just skip.

In case it wasn't already clear, this is pretty introductory stuff, Haskell-wise.  If you're already well-versed in the basics it might be fun to read just to see what you'd do differently, but this implementation is probably not gonna blow your mind or anything - in fact, I want to see yours!

### Me

Not a Haskell programmer.  I partially wrote this to psyche myself up about it again and this particular program was the biggest thing I made in my little bit of time I spent learning it a year and a half ago... you know, in the interest of full disclosure.  I'm no expert in general - reading this old code again and fully explaining it was an educational exercise.  Both writing it the first time and writing this post now taught me a lot about Haskell, so hopefully it can teach some other beginners some stuff about Haskell too!  I think Haskell is really cool and I want more beginners to play with it no matter what other language you're focusing on for the bulk of your work.  This is a non-stuffy but (overly?) thorough way to look at a small program you've probably written before in something more familiar.

While I don't believe I'm off the mark with any of the content here, if any Haskell (or otherwise) programmers notice something egregious please let me know at ben@deciduously.com.

If I've totally turned you off from reading on with *that* impressive bio, it's been a pleasure.  Enjoy your day!

As for the intrepid remainder, I think this is gonna be a good time.  You're gonna learn at least a thing, maybe two things.  You know, good clean fun.  ¡Vámonos!

### The Program

This is a dirt simple Tic Tac Toe game played on the command line against a computer opponent that just plays randomly. Fun, right? Hours if not days of entertainment await the lucky user. A project like this is usually my go-to "hello world" in a new language because at the end it demonstrates you can leverage the language's various facilities at least a little, like control flow and IO. For Haskell it was more a "TTFN, world", but the point stands. 

The full source can be found [here](https://github.com/deciduously/tictactoe-hs/blob/master/src/Main.hs), the entirety of which will appear in snippet-form below.

If you'd like to build the code locally you'll need to have `stack` installed.  See the [stack docs](https://docs.haskellstack.org/en/stable/README/) for installation instructions - if you're planning to keep exploring Haskell you'll want this tool.  It will automatically manage your GHC installations and package dependencies.

Once you've got that good to go you can open a terminal in the project directory (clone [this repo](https://github.com/deciduously/tictactoe-hs)) and run `stack setup` to install the compiler and dependencies followed by `stack exec ttt` to compile and run the executable or `stack ghci` to open a REPL from which you can interact directly with the functions defined (including `main`).  I recommend the REPL because that way you can also try each individual function on whatever inputs you'd like.  If you tweak `src/Main.hs` you can type `:r` at the prompt to recompile and load the new version (or get yelled at a little, depending how you did).  Use `:q` to quit.

Here's a sample game, as executed from the REPL:

```
*Main> main
 1  2  3
 4  5  6
 7  8  9

Your move: 2
Computer plays 9
 1  X  3
 4  5  6
 7  8  O

Your move: 1
Computer plays 8
 X  X  3
 4  5  6
 7  O  O

Your move: 6
Computer plays 3
 X  X  O
 4  5  X
 7  O  O

Your move: 37
Only one digit allowed!
 X  X  O
 4  5  X
 7  O  O

Your move: 4
Computer plays 5
 X  X  O
 X  O  X
 7  O  O

Your move: 7
 X  X  O
 X  O  X
 X  O  O

Human won!
*** Exception: ExitSuccess
*Main>
```

Suck it, random number generator.

## The Good Stuff

### First steps

Haskell programs are organized into modules.  If you’re coming from an object-oriented world, it’s not quite analogous to a class - it’s more just a way of describing a namespace. In this way you do encapsulate functionality, but not quite as rigidly as a class does as a “blueprint” for an object.  Each module consists of “entities” like functions and types, which can be imported into other modules for use.

Opening up Main.hs we see the following declarations:

```haskell
module Main where

import           Control.Monad (forever, when)
import           Data.Bool     (bool)
import           Data.Char     (digitToInt)
import           Data.List     (isSubsequenceOf)
import           Data.Maybe    (isJust, isNothing)
import           System.Exit   (exitSuccess)
import           System.IO     (hFlush, stdout)
import           System.Random (randomRIO)
```

The module is the first term followed by the functions we're importing. This program only has the one module, but if there were more Main would be a sensible place to start looking for our program entry.  All other modules listed here are available in Haskell's standard library and I'll discuss each in turn as we use it during the walkthrough.  The module name is the first part with the specific imports from that module we use listed individually in the parens.

I see some type declarations right under the import statements but I don't really understand what needs modelling yet, so instead I'm going to skim down and see which actual function is called first when you execute this (I did promise in the intro I’d do that).  In an imperative language the task is to write a subroutine telling the computer step by step how to execute a game of Tic Tac Toe.  In Haskell everything is a pure mathematical transformation.  Our task is to define a value so that evaluating it plays a game of Tic Tac Toe with you as it resolves.  Instead of how to get to the end result step by step, we're just defining what the end result is which involves a bit of a mental twist if functional programming is new to you - especially because a turn-based, IO-oriented program inherently has some imperative parts!  In `Main.hs` this value is also called `main` and lives at the bottom of the file.

Before we can dive into the code, though, the line `main :: IO ()` itself has some unpacking to do.

In Haskell every value has a type. Functions count because they evaluate to values.   The compiler doesn’t need to worry about unexpected mutation and side effects so every function can simply be viewed as equivalent its return value much more than in an imperative language.  Haskell also goes hard on the types in a way that you've likely never come across if languages like Java, C#, or C++ are as heavy a type system as you've ever worked with. The compiler is actually magic (no, really, *magic*) and does not require annotations - it's considered good style for top-level functions in a module but they can be omitted for internal values. However, they are a huge help if you start getting bogged down in compiler errors! A type annotation has the value's name first, followed by the double colon `::`, followed by the type, and you'll see them all over Haskell code.

Our main value has the type `IO ()`. Right off the bat we get a taste of some of funky fresh Haskell weirdness.  We have our regular looking types available like `Int`, but this `IO ()` is our first hint the types are set to "Maximum Cool".  Before we can talk about the logic inside this `IO ()` *thing*, we've got to get to the bottom of what exactly it even is.

#### Setting the scene: `IO ()`

I'm going to preface this by saying I am not making this a blog post about Monads if you've heard the good advice about generally running away from those. I do need to talk about them at least a little (we can keep it to 30,000 feet but there's a `(>>=)` or two just sitting there), and they're really not a scary thing at all. This is the super simple shakedown, and it's only a shakedown because I thought it sounded good after "super simple".

IO is a monad. In Haskell, Monad is a design pattern that allows us to imbue simpler types with some higher-order functionality that strictly adheres to a set of laws in order to compose them with more flexibility than a pure functional model would afford.  I'll unpack this below.  This pattern is not specific to Haskell but the Haskell compiler is able to verify these "monad laws" for us at compile time due to its powerful typeclass system.  It comes with a number of built-in instances for various useful types.  You could absolutely approximate the pattern in any language with facilities for generic programming.  I'll talk more about what typeclasses and instances are later - for now, know that Haskell has a concept of "categories of types" which it can enforce and one such category is "Monad".  Our whole program here is type `IO ()`, read "IO unit" which is a monad.

As we’ve stated, Haskell is a functional programming language which is really a rather broad category of languages that emphasize a style of programming in which the function is the basic unit of computation. In Haskell this is further constrained by demanding that every function be a pure function. If you're not familiar with the terminology "pure" means that the function does not rely upon or act on values outside of its own body. Put another way, the function will always return the same output for a given input because there is nothing else the output depends on and you're guaranteed that nothing outside of itself will change in the course of running it. The entire result of the computation is 100% determined by the arguments themselves.  This property is known as *referential transparency*, and means you can swap in the return value for the function call without affecting the behavior.  Instead of writing `x = 5 + 2`, you can say `x = 7` - these two are equivelent even though one relies on the addition functionto produce the answer.

Here's a small example in JavaScript:

```javascript
var impureIncrementAll = function(nums) {
  const len = nums.length
  for (var i = 0; i < len; i++) {
    nums[i] += 1;
  }
  return nums
}

var pureIncrementAll = function(nums) {
  const len = nums.length
  var ret = []
  for (var i = 0; i < len; i++) {
    ret[i] = nums[i] + 1
  }
  return ret
}

var a = [1, 2, 3]

impureIncrementAll(a)

console.log(a) /* [2, 3, 4] */

a = [1, 2, 3]

var b = pureIncrementAll(a)

console.log(a) /* [1, 2, 3] */
console.log(b) /* [2, 3, 4] */
```

If you were to just call `pureIncrement(a)` without introducing the `b` value to capture the result, nothing would happen.  Not completely nothing, actually - the function would still execute, it's not a no-op, but the result will just be discarded and `a` will remain the same.

Haskell doesn't have anything like `impureIncrementAll`.  You cannot do that.  This definition of "function" maps a lot more closely to the *mathematical* concept.  In most imperative languages what we call a "function" is more accurately a "subroutine", which may or may not be effectful.  Haskell doesn't have those.  The savvy among you might already be asking "but wait! There are all kinds of things a function might want to do outside of itself. How about printing a letter to the screen or responding to any external input?" To which Haskell says "Oh, shoot. We hadn't thought of that. Pack it up!" Good post everyone.

...Hah! Got you, didn't I. Monads conveniently allow Haskell to get around this little technicality of actually having to be useful by wrapping up the ugly-but-necessary effects like using `stdout` and compartmentalizing them in a purely functional way.  I read this particular type as "IO Unit". The first part means it's of type IO, so it does something with IO (Input/Output). But we need to know what type this monad returns so that we can use it within our typed functional program (spoiler alert: specifically within other monads.)  An IO monad like main will do something with IO in its body but also evaluates to (`return`s) something in the context of your purely functional program. The Monad is a way of encapsulating that idea - whatever IO it does will happen inside of it and then you get this second type back, still wrapped up as an `IO something`. A monad can be thought of as an "action" or "computation." It isn't the action itself, it's just the concept of carrying out that action. It's a noun through and through, just a "thing" we can pass around in our program (NOT a function, though functions can return Monads), but it's definitely a little weird at first. Monads turn out to be a great way to compose functionality without sacrificing that sweet, saucy purity.  All of our `console.log()`-equivalent code that handles the user/program data boundary will be neatly inside a type marked so and use that to contain it's effectfullness and not get all over everything, which would break the really nice guarantee that innocent values like `a` will never be mutated willy-nilly by rogue calls to `impureIncrementAll` when you least expect it.

That probably doesn’t sit well with you.  One possible reason is that it’s a bald-faced lie or at least a gross over-simplification.  You can't just hand-wave away the fact that somehow being in an IO monad lets us write seemingly impure code, I can hear you saying.  Hold your rotten tomatoes, please.  The Monad structure is allowing us to contextualize the world outside the program in a really handy way and pass it around.  Building up how that’s working out in Haskell is both a cool exercise to step through yourself and completely outside the scope of this tutorial.  It's best left to somebody who knows a lot more about what they’re talking about, and not at all on a need-to-know basis to utilize monadic IO.  Here, we’re using it to tag every single part of our program that does any IO - it helps me to think of that `IO` part of the type signature `IO ()` as a sort of phantom argument to your function you can’t use that’s representing the entire world outside our Haskell program.  The world outside is getting passed in to your function so you can act upon it safely, and then getting neatly passed out in its new form when you're done for the next step of the pipeline.  It’s not precisely what’s going on but its not completely unlike it either!  This pattern allows us to keep our “100% of the result of this function is determined by the arguments themselves” constraint and still interact with a user while still writing clean, easy-to-follow code.

Monads have uses far beyond just containing IO effects, but for our use in this program the IO Monad is the (slightly confusing) type of "doing input and/or output" and it will yield a thing when it’s done. The type it yields is the second term. For `main`, we don't have anything so we return something of type `()`, the empty tuple (there is only one possible value of type `()`, which is `()` - the empty tuple itself). Putting them together, we have our type `IO ()`. This is akin to `void` in C/C++, or, well, `unit` or `()` in a bunch of different languages. Zilch.

As a side note, an `IO ()` on its own doesn’t do anything unless it’s been executed inside `main`!  That’s the only way to get it to do something.

For a superior but still blitz pace Monad (etc.) usage run-through with pictures [this blogpost](http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html) will get you up to speed surprisingly quickly from the author of ["Grokking algorithms"](http://a.co/ba5icnv).  The Haskell wiki also has a number of excellent articles digging deeper into IO specifically and more general Haskell type theory goodness.

It's completely normal for this to make little sense - it doesn't need to yet.  Monads are truly not complicated at all and show up all over the place in all sorts of languages, but the way to learn how to program with them is to sit down and write some code with monads - no amount of reading is going to give you the instinctual understanding you need.  They're notoriously hard to describe without getting all category-theory on you - because really, a Monad is just a Monoid in the category of Endofunctors, what could be clearer - and I promise you you're not just one more analogy away from understanding it.  Write some code!  With that in mind, let's get back to the grizz.

### Back to the Grizz

That wasn't too terrible, right? Or maybe it was, I don't have a response mechanism from any sort of audience as I'm writing this. Let me know. I regret saying "Grizz" already, no need to mention that. It's not a real word and I've really leaned into it, I know.  I'm sorry.

Anyway, let's look back up at `main :: IO ()`:

```haskell
-- line 93
main :: IO ()
main = do
  let board = freshBoard
  runGame board
```

From the type we know it will perform IO and give us NOTHING back. Friggin' main, pull a little weight once in a while, huh?

The definition of `main` is directly below the type declaration.  Just to drive the point home once more - I’m not calling it a function.  It isn’t one, like it would be in, say, C:

```C
int main() { return 0; }
```

That C snippet defines a function that returns an int.  In our Haskell program there’s no function call.  It's just an `IO ()` - an IO action. A noun, not a verb.  A thing that performs some IO to resolve to its final value.  Similar to how if you define a binding `let a = 2`, typing `a` at a REPL will just give you back the value 2, typing `main` will just give you back the value "the action of playing a tic tac toe game", which involves user interaction to resolve.  It's a weird but important distinction.

We can tell it's a simple value because the type doesn't have an arrow `->` in it. All functions are mappings from a type to another type (or more), like `Int -> Int` for something like `int double(int x) { return x * x; }` or `Int -> ()`, like the direct translation of the C would yield - `()` is a lot like `void`. Main just does our IO and has () to show for it.

At this point I’ll note that Haskell is like Python in that its scopes are delimited by semantic whitespace.  Anything indented is inside the parent scope. Our main is going to `do` a few things.

#### Gettin Your Sequence On With `do`

`do` is actually syntactic sugar for some more monadic jazz, so as with my first digression this is not a full explanation but rather a taste - just enough to keep moving. We can use this structure inside any monad and it lets us "fake" an imperative style of programming. You may have noticed main doesn't look like what you'd think a functional program should, doing things *and then* other things all imperatively and stuff. We’re supposed to be operating in this super pure mathematical world of function evaluation and nothing else! This is not how a functional program works, it's supposed to just compose the results of other functions.

In fact, we still are.  The `do` keyword is syntactic sugar which lets you chain monads together with the `(>>)`/'then' operator, which is specific to Monads (as in, it's defined in the `Monad` typeclass, so all monads are guaranteed to work with it). Pure and strongly typed, like GHC (our magical compiler) demands. The do notation just helps it look cleaner and easier to follow while we pass around our “phantom outside world” parameter in the course of the computation through several successive IO operations.

The takeaway is that if you're in a monad like `main :: IO ()` you can generally use `do` to do some monadic things (like IO) sequentially and that's a-okay with Haskell. This is what allows monads to, for instance, respond to input based on the contents. Inside the do block both things I call out to are also IO monads. The total value of main, i.e. the result of running the executable, relies on some external input to compute and it's going to need to respond based on whatever input it receives.

Whew. Another token, another couple paragraphs of exposition. So, what is it we're `do`ing? The first statement is something I finally don't have a whole paragraph about. With the line `let board = freshBoard` we're creating a binding of the name board and assigning it the value `freshBoard`. What's `freshBoard`, you ask? Why, lines 27 and 28 of Main.hs of course!

### Leaving `main`

```haskell
-- line 27
freshBoard :: Board
freshBoard = Board $ replicate 9 Nothing
```

Alright, `freshBoard` has type `Board`.  I don't even want to know what a fresh one of these bad boys is without know what a `Board` looks like in general, so now lets go back up to the top and see what types I've defined.

```haskell
-- line 12
newtype Board = Board [Maybe Player]
data Player = Human | Computer deriving (Eq, Show)
```

And there you have it.  A `Board` is a container for a `[Maybe Player]`.  The brackets around `Maybe Player` mean that it's a list of `Maybe Player`.  Obvious, right?  I'm joking, I'll talk about it.

It makes sense to get familiar with `Player` first, since we're using it inside the definition of `Board`.  This is a union type, like an enum.  If you've never worked with those it's just a bit of data that can either be a `Human` or a `Computer` and nothing else.  We've auto-derived the *typeclasses* `Eq` and `Show` for it that let us compare `Players` for equality, i.e. tell if `Human == Human`, and to display them to the console as is.  I'll come back to typeclasses a little later, they're quite neat.  So we know that anywhere that expects a value of *type* `Player`, that *value* is either `Human` or `Computer` - there's nothing else it can possibly be.

A `Maybe` is a useful type (spoiler alert, another monad!) allowing you to encode the concept of nullablillity into the type system, instead of as a `null` value that can get thrown around.  Similar concepts appear in other languages like Rust and Swift (and OCaml and Scala and F# and SML and Elm and etc, etc - it's not a new or Haskell-specific concept is the point here), and if your language of choice doesn't come with it, you can probably handroll one yourself, albeit maybe without some of the compile-time property checking we're getting here.  A `Maybe` can either be a value of `Nothing` or a `Just <something>`, in our case a `Player` from the type.  `Maybe Player` is actually also a type - `Maybe` is a *higher-kinded type* meaning it must parameterized with a type before it can be used in the context of your program.  Putting it all together, a list of these `Maybe Player`s might look something like the following:

```haskell
[Nothing, Just Human, Nothing, Just Computer, Nothing, Just Human, ...]
```

Even though there are three different possible values for each cell, the type `Maybe Player` concisely expresses those three values and only those three values.  This constraint is now checked at compile time for you with no boilerplate needed.

The word `kind` in higher-kinded type refers to *how many layers* of parameterization this type requires.  Without a type parameter, `Maybe` is not a complete, usable type at all - every `Maybe` will carry a specific type.  This is described with *kind* `* -> *`, meaning a mapping from something to something.  When `Player` is that something it becomes the fully resolved type `Maybe Player` with *kind* `*` that can be fully evaluated in other functions.

Remember earlier when I called the compiler magic?  It goes further... `Either`, which is *kind* `* -> * -> *`takes two type-level arguments and creates curried (partially applied) types by only supplying one parameter!  For example `Either Player` is a partially resolved type that still has kind `* -> *`, and only by specifying the other type, e.g. `Either Player Int` do you have a fully resolved `Either`, which similar to how a Maybe means you've either got a `Nothing` or a `Just <sometype>`, you will either have a value of `Left Player` or`Right Int`.  This `Either` type is a neat way to manage error-handling encoded into your types.

What's super interesting to me is that these higher-kinded types work like *type-level functions*.  Try that in Java.  What else have we used that has a type that looks like this?  Why, our new best friend the IO monad of course!  It can be an `IO ()` or an `IO Int` or anything you like, but it's still an IO monad, with kind `* -> *` until it's specified further.  It turns out `Maybe` is *also* a monad, but instead of contextualizing the scary mutable outside world, we're just contextualizing nullability.  The monad-ness is going to allow us to operate on the contained type while still ensuring we're keeping it inside a `Maybe` no matter what.  There are, though, higher-kinded types which aren't monads as well.  It's cool stuff, but not at all important for this program.

Alright, armed with at least some of that knowledge we can take a look at `Board $ replicate 9 nothing`.  This is nice and neat in that even though it looks a little incantation-y, it's got a nice simple ring to it - it almost reads like a sentence, or at least pseudocode.  Before going forward you'll want to know about `$` - this is just function application with different precedence/associativity rules.  Its `Board(replicate 9 nothing)`.  It seems redundant at first but the low precedence and right-associativity let you omit parens: `f $ g $ h x  =  f (g (h x))`.  It looks funky but if I recall it felt natural pretty quickly.  Buckle up because there's a little more token soup below.  Haskell is not shy about esoteric operators.

`replicate 9 nothing` isn't too hard to tease apart.  Function application is just via spaces in Haskell (it's a function-oriented language after all), so we're calling `replicate` with the arguments `9` and `Nothing` instead of `replicate(9, Nothing)`.  And `Board` wanted a list of `Maybe Player`s.  `replicate` makes uses the first argument to decide how many "replicas" of the 2nd to make, and returns them as a list.  Which is what we said a `Board` held. Okay, cool, so a `freshBoard` is a `Board` has nine cells that *can* hold a `Player` but don't currently:

```haskell
freshBoard = [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
```

We could have simply written the above, but we get there with much fewer keystrokes using `replicate` - gott love referential transparency!  This comprises the entire data structure for the app.

Great!  So to recap, we've now stored a `Board` of 9 cells that might contain a `Human` or a `Computer` but are currently empty.  What say you we move on to the *third* line of `main`?

### The Third Line of `main`

Now we're cooking with gas!  Our `freshBoard` is ready for some killer moves.  The next line is a simple function call reads `runGame board` - easy enough.  We're going to pass our new board into the `runGame` function.  What does that look like?

```haskell
-- line 75
runGame :: Board -> IO ()
runGame board = forever $ do
  gameOver board
  print board
  putStr "Your move: "
  hFlush stdout
  n <- getLine
  case n of
    [c] ->
      if [c] `elem` map show [(1::Integer)..9]
      then do
          let n' = digitToInt c
          if openCell board n'
          then handleInput board n' >>= compTurn >>= runGame
          else putStrLn "That's taken!"
      else putStrLn "1-9 only please"
    _   -> putStrLn "Only one digit allowed!"
```

That's a bulky one.  Let's take it one step at a time.  For starters, the type itself should look familiar enough by now.  `runGame` is a `Board -> IO ()` which is to say a function that takes a `Board` and returns an IO monad carrying Unit, or nothing at all, just like `main`. We know it's a function this time because of the `->` - it's a mapping from one thing to another.

Diving into the definition we see we're going to define another `do` block but it's going to get wrapped inside a `forever`.  We already know the `$` operator is just regular old function application so everything after it in our definition is inside the `forever`.  Back at the top of the file you can see we brought it in from the `Control.Monad` module, so, you guessed it, it's a monad thing.  Luckily this one is simple - it just means we want to execute this monad forever.  I bet you already guessed that.  We're going to do whatever's inside this function over and over again until something somewhere tells us the game is over.

What's inside this function, then?  The next line immediately calls out to another function called `gameOver` and passes it the board, which right now is fresh.  Let's look at `gameOver`:

```haskell
-- line 68
gameOver :: Board -> IO ()
gameOver board@(Board b) =
  when ( all isJust b) $ do
    print board
    putStrLn "Draw!"
    exitSuccess
```

Well, that type signature should be getting repetitive.  This is another one that takes a `Board`, does some sort of IO, and doesn't pass anything back to the caller.  The token soup in the second line is just destructuring syntax.  Our `Board` is the only argument, and all `board@(Board b)` does is allow us to refer to both the whole structure as `board` (with type `Board`) as well as specifically the inside list of cells as `b` (with type `[Maybe Player]`).

The body of this function is straightforward to read.  `when ( all isJust b)` we're going to `do` something.  `when` is another thing we imported from `Control.Monad`, but it's also not scary and does what you'd expect - checks the predicate and enters the block if true.  Remember that each one of the nine cells is a type of `Maybe Player`, and a `Maybe a` can be either `Just a` or `Nothing`, using `a` as a stand-in for any type.  `isJust` is a helper predicate from `Data.Maybe` (imported, like a fine wine) that returns true if what was passed in is the `Just` variety of `Maybe`.  We passed it along with our list of cells `b` into `all`, which is like a big ol' `AND`/`&&` - it returns the false the first time it hits a false, or the whole expression is true.

To summarize, when every cell has a player in it `gameOver` will notice that it's time to pack it up and end the game.  Specifically, it will show you the board with `print` (details below) and tell you the game was a draw with `putStrLn`.  These only work in an IO Monad, both being type `String -> IO ()` and finally now I can justify all that hullaballoo about monads before we even started looking at code!  That's some good old fashioned output.  Remembering that `do` is secretly chaining together its children with a `then/(>>)`, this ends up looking a lot like your garden variety imperative, impure stuff, but never breaks any Haskell rules to do so.  It's all one big IO monad built from the inner results of calling each of these functions, which themselves return IO monads making it all work.  That's why `main` has to be an IO monad as well even though it doesn't perform any IO explicitly - it's built from functions that call functions (that call functions) that do.  When the printing is over we just `exitSuccess`, terminating the program with status code 0.

`gameOver` makes sure there's still a game to play on the board before diving in and trying to run a turn.  If we're done, the whole process ends, and if not this function doesn't do or return anything at all so `runGame` can progress.  We've just begun our journey, so when we passed in the `Board`, `all` of it was definitely not `isJust`.  In fact, this predicate kicked back a `false` when we got to the very first cell.  Moving on, what does a run of the game loop look like?

First, it looks like we `print` it out.  Groovy.  But wait!  Slow down.  How does the compiler know what a `Board` should look like?  We made that type up ourself (here's your previously promised details!)  In Haskell "printablility" is expressed as a *typeclass* called `Show`.  We've been using typeclasses this whole time - they're (to me) the whole point of learning Haskell in the first place.

#### A digression - Typeclasses: types, with class

I know I said I wouldn't go too much into it but this is fun and quick.  I'll use `Maybe` - it's got all kinds of typeclass goodness to unwrap.  We already know `Maybe` is a higher-kinded type, specifically of *kind* `* -> *`, which means it's one of those fancy type-level functions - those asterisks stand in for any type.  This syntax is used to describe the *kind*s of types.  What we didn't talk about with `Maybe` is that it's a member of several useful typeclasses like `Eq` and`Show` and `Monad`.  These apply to specific types like `Int` or `Maybe` and define what happens to them in certain situations, and as we saw above when we defined our `Player` type can be derived automatically in some cases.  You can think of them as not unlike interfaces in an object-oriented setting, but they're really so much more than just interfaces.  The compiler comes with a lot of these already implemented for built-in types and knows how to derive simple ones for us for simple types.  For instance, when you want to print a `7` to the screen you pretty much always want to simply write that numeral to stdout.  If you wanted it to instead print the word `seven`, you could implement a custom version of `Show` with that logic.  If you ask if that `7` is `==` another `7` it's reasonable to assume the compiler can tell you it, in fact, is.  Thus, the type `Int` comes ready to go with an `Eq`.  At the REPL you can use the built-in `ghci` command `:info <type>` to get a list of all the typeclasses that type implements, as well as where it's defined:

```
Prelude> :info Int
data Int = GHC.Types.I# GHC.Prim.Int#   -- Defined in GHC.Types
instance Bounded Int -- Defined in GHC.Enum
instance Enum Int -- Defined in GHC.Enum
instance Eq Int -- Defined in GHC.Base
instance Integral Int -- Defined in GHC.Real
instance Num Int -- Defined in GHC.N…
```

As expected, the fourth line of output above tells us we've indeed got an `Eq` instance on the `Int` type to work with.

### `Show` Me The Money

For union types like `Player`, we can tell the compiler to assume we just want to print out the name of the variant like `Human` or `Computer`.  But if we wanted to do something crazy we could easily just define our own instance of `Show` that has code to manipulate it.  With a more complicated type, like `Board`, we want to have that control.  Here's our definition of `Show` for `Board`, which `print :: IO ()` is currently asking for in order to evaluate.  It's defined up with our other definitions:

```haskell
-- line 15
instance Show Board where
  show (Board cs) = foldr spaceEachThird [].withIndicesFrom 1.fmap showCell $ withIndicesFrom 1 cs
    where spaceEachThird a = (++) (bool (snd a) (snd a ++ "\n") (fst a `rem` 3 == 0))
```

Oh no.  This should be good.  Or perhaps terrible.  Lets tease this apart.  That first list just says we're defining what `Show` should do for `Board`.  So every time a caller needs to `Show` a board (with `show` the function, for example, or indirectly via a call to `putStr`), it will come here and evaluate what's inside.

To define a typeclass instance you need to define the functions the typeclass requires.  `Show` is an easy one to define - there's just the one, `show a`, where in this case `a` will be `Board`.  And as you'd expect, we can see the left half of the definition agrees: this function will `show (Board cs)`, so if we pass in our `Board` newtype `cs` will refer to the list of cells inside.

Luckily, Past Ben seems to have golfed this one, the bastard, which means taking some nicely formatted, readable code and yanking the readability out to save on characters.  No comments or anything here.  To be fair, I don't think Past Ben expected Present Ben (Future Ben?) to write this post, and Haskell is a lot of fun to golf.  No matter.  That first function, `foldr`, gives me an idea what I'm getting at already.  Let's talk about folding.

I've been talking about how Haskell is *functional* and not *imperative* - the unit of computation is the function and you construct computations by composing functions.  However, I immediately threw that `do` thing at you which does kinda-sorta let you code imperatively, but that's still just a special syntax for describing a purely functional set of computations.  We're going to run in to a problem if we want to perform the same action on a list of things.  Which is exactly what needs to happen and monads won't help us now.

In a C-style language to solve this problem of printing each cell to the screen you'd iterate over the cells with something like a `for` loop.  In Haskell there's no such thing.  A loop isn't a function or a value and those comprise our whole toolbox.  But we still have to solve this problem.  Luckily Haskell provides a rich set of tools for approaching this type of problem functionally using *recursion* and the `fold` operation is a building block that makes this easier than writing it out by hand.

By the way this whole bit is not at all Haskell specific.  Recursion and folds will show up in all sorts of places, Haskell just happens to be an excellent evironment for getting familiar with how to build them.

#### A Digression on `foldr`

Folds are not an uncommon concept in mainstream languages - if you're already good and comfy with them, feel free to skip this whole bit.  If not, though, it will help to know how they work.

The way we take a collection values and make sure we do something with every member of the collection is to consume the collection recursively.  That is, we're going to pass our whole collection into some sort of function which is going to do some sort of processing.  At the end of the function it's going to call itself again with a smaller part of the list as the argument - the part we haven't processed through the function yet.  It will do this again and again, calling itself with smaller and smaller parts of the collection until the whole thing is processed.  Easy peasy.  A `fold` is a specific type of recursive function that takes in a data structure, a collection of some type, and a function to use for each member - specifically the first element of the collection on each iteration.  It eventually yields just one single value, after the list has been fully drained.  The `reduce` operation is a special case of a `fold`, if you've come across that in JavaScript or Python or what have you.

Types are one thing that I find easier to talk about in Haskell than English.  Here's the type signature for `foldr`:

```haskell
foldr :: (a -> r -> r) -> r -> [a] -> r
```

It's fine if you stared blankly at this, that's usually step one of unraveling a type signature.  They all work the same way though, so we can walk our way through.  We know this is a function that takes three arguments because everything evaluates to one value in the end - so the compiler will expect three bits of information while processing this to get to that final `r`.  Parentheses in type signatures work as you'd expect - that first part is grouped, signifying it's a single argument with the type `a -> r -> r` instead of three separate arguments.  The second unknown type is conventionally shown with a `b` - I'm using `r` to indicate it's our return type.  If you went to look this up online, you'll probably see a `b` instead.  It doesn't matter what type, it could be anything.  This second type placeholder could even be another `a` and often is, but it doesn't *have* to be for the function to be correct so we use a different letter.

The first thing is our processing function.  This itself is a function which takes two arguments.  It takes in a single element of our `[a]`, or a list of `a` types, and some value of the type that we're returning and returns a new value with our expected return type.  When you pass in one cell of our `Board` (with type `Maybe Player`) this function will give back the next accumulated result.  The next argument is a single instance of that return type - the "destination" so to speak.  We know we're going to be getting a single value from this fold, and we have a function that takes a cell and our current running result and gives us back the new result, so we can drop that cell from the next run through the recursion.  On the first run through, though, we need somewhere to deposit the result of the computation, so `foldr` asks for a container as the second argument of type `r` to apply the result to.  This initial value we pass in is going to be transformed every run through the function and is eventually what gets returned.

If this all was too abstract, here's a simple example that might look more familiar - let's fold some basic addition into a collection:

```haskell
nums :: [Int]
nums = [1, 2, 3, 4, 5]

addEmUp ns :: [a] -> r
addEmUp ns = foldr (+) 0 ns
```

That's a lot less noisy.  In this example calling `addEmUp nums` will yield `15 :: Int`.  First, I defined a `[Int]` - a list of `Int`s - called `nums`.  Then I created a function `addEmUp` which is really an alias for a specific `fold` - notice how it doesn't do anything other than specify which arguments to use with the fold.  That's why the type signature for `addEmUp` is a lot simpler - it only takes the `[a]` collection, in this case `nums`.  So our `a` is `Int`.  The first argument, the processor, is `(+)` - the addition operator.  Operators are functions and this one takes in two values and produces a third.  Let's compare to our expected type: `a -> r -> r`.  In this case `a` is `Int` and we want an `Int` at the end, so we can substitute that type in for `r` too.  If you add an `Int` to an `Int`, lo and behold, an `Int` will pop out.  So our processor, addition, has type `Int -> Int -> Int`, which fits!  It's totally fine if `a` and `r` or any two unspecified types are the same, we just note that they don't *have* to be.

Our second argument was just a `0` - an `Int`.  We've just decided that's a perfectly fine `r` type so the second argument makes sense as an initializer for our return type.  That just leaves us with `[a]`.  Thankfully we've left that part of the type intact and are passing it in as the argument to `addEmUp`!  For this simple example, the fully qualified type of this `foldr` reads: `(Int -> Int -> Int) -> Int -> [Int] -> Int`.  Just a bunch of `Int`s.

When Haskell goes to evaluate this it will start with the full collection.  When we get to the first run through the processor will grab the first cell and then look for our accumulated result.  We haven't done anything yet so it's just `0` - we told it that in the second argument.  The first value is `1`.  Our accumulator added to our base value is `1`.  Then, we recur!  Only this time we've already processed the one, so we're calling this same function again but only on the rest of the collection, and using our newly minted `1` as the accumulator instead of the base value `0`:

```haskell
foldr (+) 0 [1, 2, 3, 4, 5]
foldr (+) 1 [2, 3, 4, 5]
```

See what happened there?  We processed the one and dropped it so our collection got shorter and we have a running total.  Expanding:

```haskell
  foldr (+) 3 [3, 4, 5]
= foldr (+) 6 [4, 5]
= foldr (+) 10 [5]
= foldr (+) 15 []
= 15
```

When a recursive function tries to recur on an empty list it knows it's done and returns the final value - in this case `15`.  We've managed to iterate without looping!  Instead we folded an operation in: `[1 + 2 + 3 + 4 + 5]`.  It's almost like we replaced the commas with our operator a step at a time from right to left.  In that way, we were able to reuse the same exact function over and over again while only changing what we pass in based on the output of the previous run.  Recursion, yo.

If this sounds outrageously inefficient, calling loads and loads of functions all the time with very similar values, well, it is.  To mitigate that overhead, Haskell performs something called "[tail-call](https://en.wikipedia.org/wiki/Tail_call) optimization" which I won't detail here but essentially means that instead of allocating a new stack frame for each successive call it's able to reuse the same stack frame and substitute the new vals and then just jump execution back up, `GOTO`-style, provided the function recurs in "tail position", which means it's the last part of the function to execute.  If you're not familiar with stack frames we're getting way beyond the scope of this post - it's not required knowledge here but interesting in general and important to understand if you'd like to use a functional language in anger.  In toy programs, the elegant functional solutions are generally fine, but as your apps scale it can start to cause problems, and languages which allow a more hybrid style generally recommend you fall back to more imperative patterns at that point.  A good old `for` loop will as a rule of thumb perform better on large amounts of data than a one-liner using a `forEach` or something similar - sometimes by orders of magnitude.  In Haskell, dealing with these performance problems involves other sorts of patterns as well, as there's no `for` loop to speak of.  I recommend you do some poking around!

As an aside this example could have been rewritten: `addEmUp = foldr (+) 0` - if the argument is the final term in the definition and the argument list it can be dropped.  This process is known as an [eta-reduction](https://en.wikipedia.org/wiki/Lambda_calculus#%CE%B7-conversion) in the lambda calculus lingo.  The compiler instead sees this definition as a curried function expecting one more value.  If it gets called with that value it will fully evaluate the expression.

### The `Show` Must Go On

That digression got a little nuts but now we're armed to dive in to this bigger, messier fold.  We know its going to do the same basic type of thing as `addEmUp`.  The first thing to look for is those three elements we know we'll need: the processing function, the starting value to use as an accumulator, and the collection to process.  

The final part - the collection - is easy.  Remembering that `$` is function application we know we're going to apply this fold to `withIndicesFrom 1 cs`.   We know `cs` from the argument list is our list of cells: `Board cs`.  Then we just call a helper function:

```haskell
-- line 19
withIndicesFrom :: Int -> [a] -> [(Int, a)]
withIndicesFrom n = zip [n..]
```

This is just an alias to attach a more domain-specific semantic name to the general function `zip`.  Given two collections, `[a]` and `[b]`, `zip` gives you back a single collection `[(a, b)]`:

```haskell
let listA = [1, 2, 3]
let listB = ["a", "b", "c"]
zip listA listB == [(1, "a"), (2, "b"), (3, "c")]
-- True
```

This alias just defines the first term to pass to `zip`.  You might notice the argument list doesn't match up with our type declaration - we're expecting two arguments, an `Int` and some list, but only have one below.  This is an example of the "eta-reduction" I mentioned earlier - the second argument, namely the list to zip with, appears last in the argument list and the function body so we drop it from both.  The fully specified version would read:

```haskell
withIndicesFrom :: Int -> [a] -> [(Int, a)]
withIndicesFrom n cs = zip [n..] cs
```

The type isn't any different so always look for the types if you get confused.  They'll tell you what's up.

We're using the argument to define the beginning of a range `[n..]`, that is, `[n, n + 1, n + 2, n + 3, ...]`.  to zip with, which will have the effect of attaching an index to each element in the list.  That's all.

#### A Brief Digression on Laziness

This function brushed up on another super-cool property of Haskell that I haven't made much use of in this program but is too neat to just blow by.

You may notice that the seemingly-innocuous expression `[n..]` doesn't specify a top value.  What we've done, then, is defined an *infinite list* starting at `n` and just going and going.

In most programming languages this is quite obviously not okay.  The process would drop everything else and build this infinite list until it blows the stack and crashes resulting in a pretty shit game.  Well, more shit at least.  Haskell, on the other hand, employs *lazy* evaluation semantics.  When the compiler passes through it's perfectly content to leave that `[n..]` alone (technically it will pre-process everything to [weak head normal form](https://wiki.haskell.org/Weak_head_normal_form)) until it needs to begin the expansion - and even then it only expands *as-needed*.  In the case of `withIndicesFrom` the argument we pass it will be finite which, if you need a refresher, is smaller than infinite.  When we hit the last value of that collection to pass into `zip` we're good to go - no need to keep drilling our way through `[n..]` for indices we won't use.  Haskell just leaves it unevaluated wherever we are and moves on.

This is a pretty incredible property that allows for all kinds of patterns not possible in strict-evaluation languages but does have the side effect of making some performance characteristics difficult to reason about, as with recursion.  It's a good thing to keep in mind when writing Haskell.

#### Back to work

Moving back to the code!  As a reminder here's the first line of our `show` definition:

```haskell
-- line 16
show (Board cs) = foldr spaceEachThird [].withIndicesFrom 1.fmap showCell $ withIndicesFrom 1 cs
```

We do have a fold, but it's the *final* part of a larger *composed* function.  The composition operator in Haskell is `.`.  This particular specimen is composed of three different parts.  Writing `a.b.c x` is like writing `a(b(c(x)))`.  It's much less noisy.

Our first part, `fmap showCell`, is going to call `showCell` on each cell in our indexed list of cells `[(0, Nothing), (1, Just Human), (2, Nothing)...]` and return the result.  Lets look at `showCell`:

```haskell
-- line 22
showCell :: (Int, Maybe Player) -> String
showCell (n, Nothing)         = " " ++ show n ++ " "
showCell (_, (Just Human))    = " X "
showCell (_, (Just Computer)) = " O "
```

This function has been written to take one of our conveniently pre-indexed cells and just boil it down to a string.  We actually define the function three times - the first definition with an argument pattern that matches how it's called will be executed.  Again, cool stuff!  A few other programming languages I've tried can do this sort of syntax too and it's easy to get spoiled.  In this case there are just three possible values for any given cell, having been defined as a `Maybe Player`.  We have a separate choice for each - if nobody's played yet we `show` the index and if it's got a player we return the proper character.

This is only part of the battle though!  This gives something like `[" 1 ", " X ", " 3 ", " O ", ...]`.  The second part of our composed function calls our new friend `withIndicesFrom` again to retain our indices so we're back up at `[(1, " 1 "), (2, " X "), (3, " 3 "), (4, " O "), ...]`.

Finally, we get to the last outer function: `foldr spaceEachThird []`.  This whole part is the final outer function.  In a C-like language, we might have written this:

```c
foldr(spaceEachThird, [], withIndicesFrom(1, showCell(withIndicesFrom(1, cs))))
```

That means that now we have our collection to fold over - it's the result of everything up to here.   Our base accumulator is just `[]` - the empty list.  The missing piece is our function to fold in:

```haskell
-- line 17
where spaceEachThird a = (++) (bool (snd a) (snd a ++ "\n") (fst a `rem` 3 == 0))
```

The `where` just means we're defining `spaceEachThird` locally for this function only - it isn't needed outside of this exact context.  We could have defined it inline using Haskell's anonymous function syntax (`\x -> x + 1`) but even I must have decided that was too hard to read and split it out.

`spaceEachThird` has been defined as taking a single argument `a`.  In this case `a` is going to be our current cell - conveniently it matches what we've been using as a stand-in type.  We know the processor acts on two input values because it has type `(a -> r -> r)`, and the other one is our accumulator, so in our definition it's going to look like we're missing an argument.  That "missing" argument is the accumulator, which is just `[]` at the beginning.

The first part of the definition is `(++)`, or concatenation.  There's a clue to where our other type goes - we're going to have whatever we're doing with `a`, the active cell, on one side, and it's going to get concatenated to the accumulator.  That makes sense - it's kind of like adding an `Int` to the accumulator.  The accumulator will now hold information from both operands.  What on earth are we adding, though?

I've grabbed the `bool` function from `Data.Bool` and it's really just some control flow.  From [Hoogle](https://hackage.haskell.org/package/base-4.11.1.0/docs/Data-Bool.html): `bool :: a -> a -> Bool -> a`.  This is just a concise way to express an `if` statement with two branches, not unlike the ternary `?` in some other languages with the arguments reversed.  You give it the two potential outcomes first, the first argument being the `False` case and the second being if its `True`, followed by the predicate.  So `spaceEachThird` is just testing the final argument and using `(++)` to concatenate the middle one to the accumulator if it checks out and otherwise the first one.  Put in pseudo-something-JS-like, `(is the index evenly divisible by 3?) ? "shove a newline on me\n" : "just keep me as is"`.  The two arguments are straightforward enough - the `True` case simply inserts a newline character `\n` to the cell's string which is the second/`snd` value in the list item.  The predicate isn't to hard to understand either - the backticks make the `rem` remainder function into an infix function and we're using `fst` to get just the index of the cell - every third cell this will be true, so we'll start a new line.  Now we get our flat 1-D list of nine cells nicely squared away (nyuk) and printed as 3 rows of 3.

### Gathering Input

Whew!  That `print board` line turned out to be a little intense.  Luckily, the beauty of programming is that we've only gotta tell it how once, right?  The next order of business is going to be asking the player where they'd like to play.  The next two lines are familiar enough:

```haskell
-- line 79
putStr "Your move: "
hFlush stdout
```

`putStr` is going to simply send its input to stdout and `hFlush` flushes the stdout buffer (ensuring the full string is printed out) before advancing to the next instruction.

Immediately following, we've got `n <- getLine`.  `getLine` waits for stdin and returns the contents when we get a `\n`, which we store to a local binding `n`.  We can do this inside of our `do` block so that the value passed in is available to the rest of the function.  As soon as the user enters a line of text and hits enter, we'll move on.

Now we get to the big `case` statement of `runGame`.  This is where we appropriately choose an action based on what the user entered.  This control flow construct is not at all dissimilar to a `switch` in other languages - more similar to a `match`, actually, in terms of expressive power.  We're going to match the value we just received from the user against a few patterns to see how to handle it.

We'll start with the outer layer:

```haskell
-- line 82
case n of
    [c] ->
      -- do some really awesome stuff with our single char
      -- ...
    _   -> putStrLn "Only one digit allowed!"
```

This syntax just checks if our input `n` consists of a single character.  If it does, we'll keep it and do stuff, and if not we'll lightly admonish the idiot at the keyboard with a `putStrLn` call - this is a `putStr` that includes a trailing newline.  

That pattern matching works because of how Haskell thinks about lists.  Instead of being an array, they're represented as a series of elements attached to each other (if you've LISP'd, it's a cons cell), with an empty list at the end of the chain - something attached to a list of somethings:

```haskell
[1, 2, 3, 4, 5] == 1 : 2 : 3 : 4 : 5 : []
```

This is why recursive functions work so well - it's easy to pull apart lists and detect the base case because they're already sort of pulled apart like that under the hood.  In pattern destructuring syntax you can get at the "head" (first element) and "tail" (everything else) with syntax like `(x:xs)`.  Here `x` is your head (`1`) and `xs` is everything else (`[2, 3, 4, 5]`).  You can get the first two with something like `x:y:xs`, match a two-element list with `x:y:[]` etc.  All lists in Haskell work like this.

In this example, we're using `[c]` - there's no `:cs` matching the tail.  This means this pattern will only match on a single-element list.

This is the last line of our function - but it's all wrapped up in a `forever`, so if we do get garbage and yell at the user we'll just take it again from the top of `runGame` until the user gives us something we can work with.

If the user complied and only passed in a single character we still have a little work to do:

```haskell
-- line 84
if [c] `elem` map show [(1::Integer)..9]
then do
  -- We've got us a digit!  Do some awesome stuff with it
  -- ...
else putStrLn "1-9 only please"
```

`if` in Haskell works more or less how you might expect with the caveat that it's an *expression* as opposed to a *statement* - that is, the entire `if` block must reduce to a value. Remember, a value with type `IO ()` counts - it's just a value of type "doing some IO" with nothing being passed back into the function.  Once consequence of the expression-like nature of the construct is that you cannot have an `if` without an `else` to execute of the predicate doesn't pass.  Aside from that, though, it's as expected - you pass in a predicate and if it evaluates to `true`, we'll execute the `then` block and if not, we'll use `else`.  If you have more than two cases I recommend `case` over `if`.

The first thing to check is whether or not the single character we now know we have is a valid play or not.  It must be a digit from 1 to 9 and not a letter or a bit of punctuation or anything else.  The first line defines this predicate using the `elem` function which checks if the first operand is an element of the second.  Most functions in Haskell are *prefix* in that the function names come first followed by the arguments.  To use a function of two arguments more like an *infix* operator between two operands, you can wrap it in backticks as in the snippet.

This predicate is asking if our char input is a digit from 1 to 9 and employs a handy little trick to do so.  We can't simply ask if `"1" == 1` because one is a `String` and the other is an `Int`.  First we need to get a list of valid chars (`["1", "2", "3", "4", "5", "6", "7", "8", "9"]`) to compare against.  A quick way to build this array is our good friend `show`.  We saw this one in action up in our typeclass hullaballoo.  This is how we convert a type into something we can print out on the screen.  In the case of an Integer this means turning it into a string representation first to send to stdout.  We can `map` the `show` function over a list `[1..9]` and it will perform that conversion for us for every element.

### Functor?  I hardly know her!

I haven't used `map` yet.  You may see `fmap` or `<$>` in other Haskell code and they all mean the same thing - return the result of calling the passed-in function on each element of a collection - actually, anything that has an instance of the typeclass `Functor` - familiar if you read that "Monads with Pictures" link!  Functors are things that keep their structure when applying functions to their contents - there's a set of "functor laws" like the "monad laws" with a different set of guarantees about what kinds of functions we can call on these types and how they'll behave.  It's a related concept to "monad" and you can tell this is just the tip of the category-theory iceberg here.

Lists are functors like the one we're `map`ping over here, and our buddy `Maybe` is a functor too.  It might be a little weird to think about mapping a function over a `Maybe`, but you can write:

```haskell
couldBeAnInt1 :: Maybe Int
couldBeAnInt = Just 7

couldBeAnInt2 :: Maybe Int
couldBeAnInt = Nothing

couldBeALargerInt = fmap (+1) couldBeALargerInt1
-- Just 8

couldBeALargerInt = fmap (+1) couldBeALargerInt2
-- Nothing
```

Because `Maybe` is a `Functor`, it knows how to handle having a function called on it no matter what the contents are and it will retain its structure on the other side having applied the function to any elements it can.  It's not unlike doing it to a list, really - it just will only have 0 or 1 things to apply the function to.

Learning about how these categories of types work and relate to each other is worth the effort - it's an entirely new level of abstraction to leverage.  If you're already familiar with the concept, you can probably see how it's an extension over generic programming, a higher level of organiziation and classification for it.  Haskell is an excellent environment for exploring the theory and how it relates to more robust real-world application code, and there's nothing stopping you from carrying the concept over to your day job - try writing the Maybe<T> functor as a class and using it instead of `null`.  The difference will be that ensuring your class adheres to the functor laws is up to you whereas Haskell can do that for you out of the box.

Anyway, unlike folding where we're using our function to generate a new value from a collection this one keeps the shape of the old collection intact in our new return value.  This is also a common functional tool in many languages.

We're using the range operator `..` to construct our list, and by tagging the first element with a concrete type `1::Integer` we ensure each element we're mapping `show` over is an integer to begin with.

With the predicate out of the way we've now determined whether or not the input stored in `n` is a single digit.  Our else statement looks like the previous - print out a quick error telling the user how exactly they were dumb and that's it - head back on up to the top of `runGame` and hope this chucklehead learned their lesson.  If it was a digit, however, we can move on to one final nested `if`:

```haskell
-- line 85
 then do
          let n' = digitToInt c
          if openCell board n'
          then handleInput board n' >>= compTurn >>= runGame
          else putStrLn "That's taken!"
```

I included the top `then` line to show that we open a new `do` block - `then do` isn't a special syntax, it's just a `do` inside a `then`.

First we save the integer version of our input `c` as a new local binding called `n'`.  Then we have one final predicate - before we can go thrusting the play's move onto the board, the Laws of Tic Tac Toe state that you can only make a move on a square if it's empty.  No playing on top of each other!  We verify this with `openCell`:

```haskell
-- line 30
openCell :: Board -> Int -> Bool
openCell (Board b) n = isNothing $ b !! (n - 1)
```

This is a function that takes two arguments, a `Board` and an integer, and returns a boolean like a predicate should.  We're going to pass in the full board and a specific square and `openCell` will tell us if the space is already occupied.

Thanks to Haskell's operator love affair this looks a little more complicated than it is at first glance.  We've seen `$` before - it's function application.  The other funky operator is `!!` - this is just a list subscript.  In a more C-like language, we might have written this exact logic something like `isNothing(b[n - 1])`.  That is, we're asking for the `n - 1`th element of our inner board list `b` (named so via destructuring in the definition: `(Board b)`), and passing it to `isNothing`.  `isNothing` we brought in at the top from `Data.Maybe` and itself is just a predicate which is true if the `Maybe a` passed in is a `Nothing` as opposed to a `Just a`.

We initialized our board to a list of `Nothing`s.  The first time through this loop any digit we pass in is going to come up clear.  If there had been a `Just Human` or `Just Computer`, we'd hit the `else` block, yell at the user a little (I mean honestly, we JUST printed out the board, get with the program), and take it from the top.

HOWEVER!  If `openCell` comes back `true`, we've finally done it - we've ensured the value passed to `n` from stdin is a value we can meaningfully use as the player's next move - it's not only a valid input, but it's a valid play.  Hot diggity dog!

The full `then` block reads:

```haskell
handleInput board n' >>= compTurn >>= runGame
```

This is three separate function calls wrapped up together with `>>=`, which is read `bind`.  This is one of the functions that's required to define as part of the `Monad` typeclass instance along with `return`, so all monads have this behavior.  We'll talk about `bind` first and come back to `return` below - but they're really two parts of the same idea.  `>>=` is going to allow us to pass the result of a monad as the input to a subsequent monad in the chain while still keeping it wrapped up in the proper context, in this case `IO a` or specifically `IO Board`.  We want to do stuff to that `Board` without losing the `IO` wrapping.  I think this is clearest through example, and luckily we're working through an example right now!  The first function call is `handleInput board n'` so let's unpack that first.

### Making a Play

I bet we can work out the type of `handleInput` from the call.  `board` is easy - it's a `Board` - and `n'` is our newly converted integer from stdin.  So we know this will be a `Board -> Int -> something`.  What, though?

Well, we know we're inside an `IO` monad, and in a series of calls chained together with the monadic `>>=`.  So it's a safe bet this will be another `IO a`, that is, an `IO` monad with some type as a result.  And if we look down the chained call, we end things up with a call to `runGame`.  We've already looked at `runGame` (we're inside of it RIGHT NOW), so we know it's a `Board -> IO ()`.  We're calling it here with no argument, but from the type know it will need a `Board`, and we're passing a monadic result through a chain of functions - so it would follow that the type of each step *must* be `IO Board`.  Lo and behold:

```haskell
-- line 46
handleInput :: Board -> Int -> IO Board
handleInput board n = do
  let b = playCell board n Human
  checkWin b Human
  gameOver b
  return b
```

Just as expected!  In the body of the function we're opening another `do` block and as our first step creating a new binding `b`.  Time to finally examine `playCell` and get this game off the ground:

```haskell
-- line 33
playCell :: Board -> Int -> Player -> Board
playCell (Board b) n m = Board $ take (n - 1) b ++ [Just m] ++ drop n b
```

From the function call we expected a type like that - 3 arguments.  We also now see it will give us back a `Board` to store in `b`. The only type we haven't seen used much yet is `Player` - but we know all about that already from discussing `Board`!  The value can be a `Human` or a `Computer` and nothing else, and in this case we're processing the human's input - so we just pass in `Human`.

In the argument list, we've destructured the `Board` again to access the list of cells inside and assigned letters to the other two.

Now, in a C-style language, you'd probably at first approach this task of adding a play to the board by indexing into the list and changing the value inside.  In Haskell, that's a big nope.  Remember when we discussed purity?  That would involve *changing the state of the world outside of the function* - namely the `Board`.  If we did it this way, this function of have wildly different and unpredicable results based entirely on the state of the `Board` when it was called, which is terrifying.  We cannot definitely look at that function and tell you what *exactly* it will do.  But, of course, this would be a dumb(er) game if nothing was ever allowed to change.

The way we get around this restriction in the functional paradigm is to not attempt to change anything at all.  Instead, we're just going to construct a *brand new* `Board` based on the previous one - like what the Javascript snippet did in `pureIncrementAll` up in the beginning.  Haskell is garbage-collected so the old iteration will be automatically dropped by the runtime with no need to call any sort of destructor or free the memory yourself.  That way the game as a whole can continue in a new state and we haven't broken our purity restriction.

I do this using the super handy `take` and `drop` functions which return new sublists leaving the input list untouched.  `take` returns the specified number of elements from the front, and `drop` returns the end of a list beginning at the index specified.  So in `playCell` I just `take` the cells up to but not including the cell specified, and at the end we'll attach the rest by `drop`ping the beginning.  That only leaves the single cell in question.  Because the `Board` requires each cell to be a `Maybe Player`, we can wrap our `Human :: Player` value inside a `Just`.  We then put it in brackets to make a single element list which we can concatenate (`++`) to our bookend sublists, and wrap the new list up in a new `Board`.  The end result is a `Board` value shaped just like the last except the cell we passed in as an argument has a `Just Human` now instead of a `Nothing`.  Everything else is a direct copy.

This way for the same inputs we can always guarantee the same outputs.  The current state of the `Board` is passed directly into the function which allows us to generate the next state appropriately, and we know exactly what it will look like given all the inputs we've got locally available.  This makes reasoning about the flow of logic in Haskell code almost trivially easy in cases that become very convoluted otherwise with shared mutable state.

### Winners Only, Please

Now that we've stored our shiny new Board with one cell updated we've got to see how well we did.  The next line of `handleInput` calls `checkWin`:

```haskell
-- line 57
checkWin :: Board -> Player -> IO ()
checkWin board@(Board b) m =
  let
    bi = withIndicesFrom 0 b
    plays = map fst.filter ((Just m==) . snd) $ bi
  in
   when (foldr ((||) . flip isSubsequenceOf plays) False winStates) $ do
     -- End the game!
```

Okay, this is a little bigger.  It's a function of two arguments returning an IO monad which (I really hope) makes sense by now.  This monad isn't returning anything (note that we havent stored a result to a binding with `<-`, we just called it inside our `do` block - which is to say our `then`/`>>` chain of monads), so `IO ()` is appropriate.  This will just do some IO and will be responsible for terminating the process if we find a win.

The `let...in` syntax is a way of creating function-local bindings not unlike `where`.  In fact, they can often be used interchangeably and the difference is subtle: `let...in` is an expression, which can be used anywhere at all that expects an expression (kinda like `if...then...else`), whereas `where` is a syntactic construct that only come after a function body.  I'm not going to get into the subtlies here, see the [Haskell Wiki](https://wiki.haskell.org/Let_vs._Where) for a more thorough discussion.

Anyway, before diving into the endgame checking we're going to set up some computed local bindings to make our life a little easier:

```haskell
-- line 59
 let
    bi = withIndicesFrom 0 b
    plays = map fst.filter ((Just m==) . snd) $ bi
  in
  -- uber cool codez
```

We've saved as `bi` a version of the `Board` we're working with zipped up with indices using our old friend `withIndicesFrom` - instead of, e.g., `[Nothing, Just Human, Nothing...]` we have `[(0, Nothing), (1, Just Human), (2, Nothing)...]`.  We're going to use this in our next `let` binding, `plays`.  This line is a little token-soupy, but we're intrepid as heck.  It's a call to `map`, and the collection (functor) we're mapping over is the newly defined `bi`, so all that junk in the middle must be our mapping function.  Let's see if we can untangle it.

This function has opted for concision via the `.` composition operator we saw up in our `Show Board` instance, at the cost of readability.  This one actually has a composed function inside a larger composed function for extra goodness.  These are easiest to read inside-out (Lisp-ers know what's up).

The first action that happens to `bi`, our indexed `Board`, is `filter ((Just m==) . snd)`.  The filter function first calls `snd` on each element returning just the second element of the tuple:

```haskell
snd (1, Just Human) == Just Human
```

Then, `(Just m==)` compares it to the value passed in as `m` - remember when we called the function, it looked like `checkWin b Human`.  We're specifically checking if the Human player won the game with their latest play.  This is why we derived the `Eq` typeclass up in the `Player` declaration - this check wouldn't compile otherwise.  So `((Just m==) . snd)` will return true on a `(Int, Maybe Player)` if the second value is `Just Human`, and false otherwise.

Now that we've pared down `bi` to only the cells that have been played we pass that whole result into `fst` - that is, grab the first value of each tuple.  These are our indices.

The end result that's stored in `plays` is a list of the indices from 0 of all of the places the Human has played.  For example, running it on cell list `[(0, Nothing), (1, Just Human), (2, Just Computer), (3, Nothing), (4, Just Human)]` will come back with `[1, 4]`.  Lovely.

Now that we've got our packed-up Human plays we can check to see if that constitutes a win.  The main body of the function, following the `in`, is another `when ... do` shindig like we saw back in `gameOver`.  This monad will execute its body under this condition, and otherwise its a no-op.

How about that condition, then?  Let's see:

```haskell
`foldr ((||) . flip isSubsequenceOf plays) False winStates
```

Aha, it's our good old friend `foldr`.  I unabashedly love folding.  True to form, we've got three arguments: a transforming function, an initializer, and a collection.  We've looked at two folds before - the trivial example used an `Int` as an initializer that we added numbers to, and the the code from the game used a collection (that we pre-built).  This time around it's simple a `Bool` - `False`.  That's is a-ok too as long as your transforming function returns a `Bool`!  It can be any type at all.  That means this whole fold will return a `Bool` - by definition, the fold always returns the same type as the initializer: `(a -> r -> r) -> r -> [a] -> r`.  And that's what we want because `when` expects a predicate.

Before picking apart the transformer let's look at `winStates` - the collection we're folding over.

```haskell
-- line 53
winStates :: [[Int]]
winStates = [[0, 1, 2], [3, 4, 5], [6, 7, 8], [0, 3, 6], [1, 4, 7], [2, 5, 8], [0, 4, 8], [2, 4, 6]]
```

This is pretty simple - it's just a list of lists.  This is admittedly not an elegant way to handle this problem, but TicTacToe is simple enough that it's feasible to simply hardcode all the possible winning configurations.  This is a list of lists of `Int`s (`[[Int]]`) just containing all the indexes that are in a row.

Finally, the transformer: `(||) . flip isSubsequenceOf plays`.  We know this whole bit of code is a function of type `(a -> r -> r)` - filling in the concrete types this becomes `([Int] -> Bool -> Bool)` - out initial collection is a `[[Int]]`, a list of lists of `Int`s, so each time through we're checking just one of these sublists and returning true or false.

The workhorse function I chose is the aptly-named `isSubsequenceOf`, imported (with love) from `Data.List`.  It returns true if the elements of the first list appear in order (but not necessarily consecutively) in the second list.  The docs helpfully note that this function is equivalent to calling `elem x (subsequences y)` - the standard library is building useful abstractions by composing smaller abstractions!  I actually came across this library function in the course of researching a problem I came up against trying to implement it myself.  I don't remember the specific nature of the problem I had because all it took to solve was a look at the standard library.  Don't forget to look through it for functionality you need before falling down the wrong rabbit hole.

#### Typclass constraints - a digression

According to [Hackage](https://hackage.haskell.org/package/base-4.11.1.0/docs/Data-List.html#v:isSubsequenceOf), this function has type `Eq a => [a] -> [a] -> Bool`.  This signture has one syntactic element I haven't touched upon yet - that first part, `Eq a =>`, is a *typeclass constraint* on `a`.  I've been using `a` as a stand-in for "any type" over the course of this article.  This syntax lets you more precisely define what sorts of types are ok - unlike a fold, `isSubSequenceOf` only makes sense to call on lists with elements that can be compared to each other.  This stands to reason - it's going to have to check each element in one list against the other.  This is Haskells system for *ad-hoc polymorphism*.  If the types involved do not have instances of the typeclasses specified, either derived or hand-implemented, this won't compile.

### `flip`ing out

The last unfamiliar part of this function composition is the word "flip".  This is a simple but useful function that just switches the order in which the arguments are expected.  The way we're calling it in our transformer function, `isSubsequenceOf` receives our `plays` list first and then the element of `winStates` the fold is currently processing.  However, we want it the other way around.  To tell if we've won we want to check if the winState is a subsequence of all the plays this player has made.  You can win with other non-lined-up plays on the board, they're just irrelevant.  `flip` just swaps the positions of the arguments so we get the logic we want!

Finally, we compose (`.`) that result with the simple operator `(||)`.  This is usually used infix, e.g. `true || false`, but we can use it as a normal prefix function as well by wrapping it in parens.  One value it receives will be the result of our `flip isSubsequenceOf` call, and the other?  Why, that's our initialized `Bool`!  By chaining together all these calls with a big 'ol `OR`/`||`, this transformer will return `True` for the whole collection if any one of these iterations comes back `True` (meaning `plays` contains one of our `winStates`) or remain `False` as we initialized it.

If it was `False`, we didn't win and `checkWin` has nothing else to do.  The code inside the block doesn't execute, we have `()` to return, and control passes back to the caller.  If we *did* win:

```haskell
-- line 63
print board
putStrLn $ show m ++ " won!"
exitSuccess
```

Now we can finall hop back in and finish up `handleInput`:

```haskell
gameOver b
return b
```

If we've gotten here, it means `checkWin` didn't find a winning board configuration, so before we move on we call `gameOver` again to see if this play resulted in a draw, and if not, we `return b`.  `return` is a little different than you're used to - it specifically means to re-wrap our `Board` type in our `IO` context.  This is the other part of `>>=` - it's how we're passing this result around with the gamestate through successively chained IO actions.  This is how we pass the result back to the main `runGame` loop having determined that this play didn't end the game in either a win or a draw.

### RNG Rover

We're nearing the end of the road - if you're still with me, I'm seriously impressed!  We've just got one last part to pull this together.  After all, what's a game of TicTacToe without a steely-eyed, calculating oponent ready to squelch your every plan?

Well, we're not going to find out here because my computer player is real dumb and plays by dice roll.  It could be fun to try to make a smarter one - I'm leaving that as an exercise to the reader (read: too lazy to do it myself).

Rewinding a little, we entered `handleInput` inside this larger clause:

```haskell
-- line 87
handleInput board n' >>= compTurn >>= runGame
```

So far, we've updated the world state according to human input, made sure there's still a game going on, and received the new `Board` to work with.  Now we're going to pass that brand new world state into `compTurn` via `>>=` which as we discussed will allow the `Board` to be passed without losing the `IO a` context it started with.  This means we should expect `compTurn` to take a `Board` as input and, because we're in the middle of a `>>=`/`bind` chain, return an `IO Board`:

```haskell
-- line 36
compTurn :: Board -> IO Board
compTurn board@(Board b) = do
  let options = filter (isNothing.snd).withIndicesFrom 1 $ b
  r <- randomRIO (0, length options - 1)
  let play = (fst $ options !! r)
  let b2 = playCell board play Computer
  putStrLn $ "Computer plays " ++ show play
  checkWin b2 Computer
  return b2
```

Great.  This function is mostly familiar by now.  We see our `IO Board` return type, we're destructuring the argument to get at the list of cells as `b`, we've got our old friend the `do` block - nothing too surprising.

The first line creates local binding `options`, which is going to be the result of `filter`ing our list of cells.  Filter will return a collection containing only those elements of the input collection for which the predicate is true.  Again, aptly named.  Let's take a look at the predicate:

```haskell
(isNothing.snd).withIndicesFrom 1
```

This function is composed from parts we've seen before.  First we're going to zip up our cells with indices starting from 1 (spoiler alert, because that's what `playCell` wants as input).  Then we're going to pass that to the composition `.` of `snd` and `isNothing`.  Hopefully this starts to feel a little more readable by now - in English, this `filter` will have the effect of storing to `options` a list of 1-indexed cells that contain a `Nothing` - anything that's a `Just Human` or `Just Computer` will be omitted.  These comprise the possible cells the computer can choose for its next play.

In the next line we introduce the randomness.  This ends up looking similar to how you'd do this in the language of your choice.  `randomRIO` from `System.Random` takes a range and will give you a pseudo-random number in that range.  We're using the length of our `options` list, and storing the result to `r`.

Now we've got to actually make the change.  This is done with `playCell` again - the differences being that instead of user input, we're using `!!` to index into `options` with the random number we just grabbed and we're passing in `Computer` instead of `Human`.  Now `b2` holds our new `Board` with the random play applied.  Afterwards we can just inform the user where the computer went. With all that taken care of, we can see if the computer managed to win the thing with `checkWin`.  If it did `checkWin` will handle ending the game for us and if not, we `return` the new gamestate again.  No need to call `gameOver` again here because `runGame` does so first - and our pipeline `handleInput >>= compTurn >>= runGame` is sending us right back up there to start the next turn.

### The Thrilling Conclusion

We did it!  I'm all out of code to unpack.  `runGame` has everything it needs to alternate human turns and computer turns until somebody wins or we run out of spaces.  Haskell ain't no thang :)