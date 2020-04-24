# Addendum for Practical FP in Scala

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->


- [Introduction](#introduction)
- [Prequisites and Chapter 1](#prequisites-and-chapter-1)
  - [Type class recap](#type-class-recap)
  - [Type class coherence](#type-class-coherence)
  - [Higher kinded types](#higher-kinded-types)
  - [Kind projector](#kind-projector)
  - [Referential transparency](#referential-transparency)
  - [IO Monad](#io-monad)
  - [Further reading](#further-reading)
- [Chapter 2](#chapter-2)
  - [Effectful computations, `F[_]`](#effectful-computations-f_)
  - [Applicative and monadic composition,  `*>` , `>>`](#applicative-and-monadic-composition----)
  - [Sequential vs concurrent state](#sequential-vs-concurrent-state)
- [References](#references)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Introduction

This text is based on common questions and comments we get when onboarding existing Scala programmers to functional code bases in Tapad. It's presented as a commentary and addendum to the book [Practical FP in Scala](https://leanpub.com/pfp-scala); this book does a good job of explaining and demonstrating today's typical typelevel-stack-based Scala application. Discussions and questions from our internal bookclub on this book is repeated in this document for the benefit of future readers.

The book states in its prerequisites that  "This book is considered intermediate to advanced" and that the reader is required to be familiar with the following topics:

* Higher-Kinded Types (HKTs)
* Type classes
* IO Monad
* Referential Transparency

We endevour to lower the bar with this text. Our goal is that if you know your way around Scala, you can go through the shopping cart example in "Practical FP in Scala" and then be fully productive in code bases written in a functional style.

This is if course not a substitue for learning functional programming fundamentals, but we find that requiring new team members to go through the [Red Book] then [Scala with Cats](https://underscore.io/books/scala-with-cats/), and then maybe "Practical FP in Scala" is neither realistic nor practical. We strongly believe that everyone can appreciate and be productive in functional code bases without a high up-front investment in understanding the fundamentals.

# Prequisites and Chapter 1

## Type class recap

I like to do a quick recap on _polymorphism_ before explaining type classes. The wikipedia definition serves us just fine:

> In [programming languages](https://en.wikipedia.org/wiki/Programming_language) and [type theory](https://en.wikipedia.org/wiki/Type_theory), **polymorphism** is the provision of a single [interface](https://en.wikipedia.org/wiki/Interface_(computing)) to entities of different [types](https://en.wikipedia.org/wiki/Data_type)[[1\]](https://en.wikipedia.org/wiki/Polymorphism_(computer_science)#cite_note-1) …

So polymorphism is the association of an _interface_ with a _type_, and there might be many such associations. Let's take an example that should be familiar  for everyone:

```scala
trait PrettyPrint {
   def pretty: String
}
```

and then

```scala
class Elephant(name: String) extends PrettyPrint {
  def pretty: String = s"An elephant named $name"
}
```

So `PrettyPrint` is our interface and the implementation is 

```scala
def pretty: String = s"An elephant named $name"
```

Here's the point: The fact that we placed the implementation in the _body_ of the class `Elephant` was just because we chose to _subtype_ `PrettyPrint`. We chose _subtyping_ as the mechanism for associating an implementation of the interface with the type `Elephant`. How else could we express this association?

We could've made the "non-interface part" of the association a type parameter to the interface. So we would change `PrettyPrint` to

```scala
trait PrettyPrint[A] {
  def pretty(a: A): String
}
```

with this encoding, we could express the association above as

```scala
val prettyElephant: PrettyPrint[Elephant] = new PrettyPrint[Elephant] {
  def pretty(a: Elephant): String = s"An elephant named ${a.name}"
}
```

The two examples above are called _subtype polymorphism_ and _ad hoc polymorphism_ respectively. One obvious difference is that in order to express polymorphism using subtyping you need to alter and compile one side of the association. This is not necessary with the ad-hoc encoding: you may define an interface and implementations for types that are already compiled without recompiling them, hence the name "ad-hoc".

You may wonder how you could easily use implementations encoded with ad-hoc polymorphism. For Scala specifically the answer lies in the implicit mechanism. We will come back to this when discussing the finer details of Scala's implicit scope.

Type classes are interfaces expressed with the ad-hoc encoding with the additional constraint that they are _coherent_. 

## Type class coherence

One maybe not so obvious property of ad-hoc polymorphism is that it allows for implementing the same interface multiple times for the same type. For example:

```scala
// Using Scala 2.12 function syntax for traits with a single def
val prettyElephant: PrettyPrint[Elephant] = a => "An elephant named ${a.name}"
val veryPrettyElephant: PrettyPrint[Elephant] = a => "-=- An elephant named ${a.name} -=-"
```

_Coherence_ refers to a situation when

> … every different valid typing derivation for a program leads to a resulting program that has the same dynamic semantics.
>
> Simon Peyton Jones et.al [1]

A _type class_ is an interface that allows for ad-hoc polymorphism that is also coherent. From what we've gone through so far, we can say that a type class is an interface `I[_]` where there is at most one implementation `I[A]` for any type `A`. 

## Higher kinded types

As with type classes and polymorphism, it's useful to do a quick recap on types and kinds before discussing higher kinded types.

Types are, without getting too technical, a lot like sets. `Int` is the set of integers, `String` is the set of Strings, `List[Int]` is the set of lists of integers, and so on. Let's refer to these examples of types as _value types_ and distinguish them from other type constructs in that you can assign a value to them. In other words, they can appear on the left hand side of a `val` assignment as a type annotation:

```scala
val a: List[Int] = List(42, 43)
```

What types can you not assign a value to? Well, `List` is one example, `Either` is another. That is, you might not write

```scala
val a: Either = … // There's nothing that will make this code compile
```

These type constructs needs one or several parameters, which are also types, in order to get a value type that we can assign values to. So for example `List[String]` is a type, but just `List` isn't. We refer to `List` as a _type constructor_, it's one of several _kinds_.

We categorize kinds very much like we categorize functions. Just like all functions extend `Function0`, `Function1`, `Function2` and so on—in other words categorized by the number of parameters they take, we categorize kinds by their _shape_. Let's see some examples:

| Kind               | Scala shape                  | Examples                                                |
| ------------------ | ---------------------------- | ------------------------------------------------------- |
| *                  | `_` or just `A`              | `String`, `Option[Int]`, `Elephant`, `IO[HttpResponse]` |
| * -> *             | `_[_]` or `F[_]`             | `Option`,` List`,` IO`                                  |
| * -> * -> *        | `_[_, _]` or `F[_,_]`        | `Either`, `Map`, `Ior`                                  |
| (* -> *) -> *      | `_[_[_]]` or `F[_[_]]`       | `Monad`, `Functor`, `UserService[F[_]]`                 |
| (* -> *) -> * -> * | `_[_[_], _]` or `F[_[_], _]` | `cats.effect.Resource`                                  |

The star here is not to be confused with the star in the kind projector in the next section. The star here is the formal syntax for kinds, you'll encounter this in texts on kinds. We include it here for completion, but it's the first and last you'll see of it in this text. The "scala shape" column is the encoding Scala uses. It's not the easiest to read!

You'll recognize kinds as the mechanism we use to abstract over types. For instance, in the definition `class List[A]` we're restricting the _shape_ of the type parameter to `List` to that of `_` (or `A`). You can pass it anything from the top row in the table above, but not any rows below because they don't belong to the correct kind; `List[IO[HttpResponse]]` compiles, but `IO[Option]` doesn't.

The shapes in the bottom two rows are examples of what we refer to as "Higher kinded types". They are characterized by taking not a value type (e.g. `A`) as a type parameter, but another kind. What is the use of this? It lets us create interfaces that abstracts over other kinds, think ad-hoc polymorphism, example:

```scala
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}
```

Remember that that with ad-hoc polymorphism we take the implementing type as a type parameter. The interface above defines a `map` method, and it can only be implemented for types that has a hole. For example, `List`, `Option` and `IO`, but not `Elephant`, `Either` or `Monad`. We use the type class encoding for polymorphism here because there is no alternative in Scala! That is, there is no way we can create an interface and have a restriction that subtypes of it must have a hole. In languages that doesn't support higher kinded types, this kind of abstraction is not possible to express.

Here's an exercise for you: open a REPL and define a function with a type parameter, play around with what types you can pass to it (I use [Ammonite](https://ammonite.io) below). Try out the types in the table above:

```
@ def foo[F[_,_]]: String = "foo"
defined function foo

@ foo[Either]
res2: String = "foo"

@ foo[List]
Compilation Failed
```

## Kind projector

The Scala [kind projector plugin](https://github.com/typelevel/kind-projector) is a compiler plugin that you might encounter in the wild, and that you do encounter in the "Practical FP in Scala" book. It looks like this

```scala
class Users[F[_]: ApplicativeThrowing[*[_], Throwable]]
```

Frankly I don't think the kind projector is something that you will use or encounter much – it's common to use type aliases instead – but with the knowledge of kinds in place, it's quite easy to grasp.

The kind project plugin lets you turn a type with shape `F[_,_]` into a type with shape `F[_]` by "fixing one the holes". It generalizes, so you can "fill in the holes" of kinds of any arity and project the resulting kind. Here's some examples from the official readme:

```scala
Tuple2[*, Double]        // equivalent to: type R[A] = Tuple2[A, Double]
Either[Int, +*]          // equivalent to: type R[+A] = Either[Int, A]
Function2[-*, Long, +*]  // equivalent to: type R[-A, +B] = Function2[A, Long, B]
EitherT[*[_], Int, *]    // equivalent to: type R[F[_], B] = EitherT[F, Int, B]
```

You place the `*` where you want the new holes to be; the new holes will have shape `_`. If you need to make a hole with shape `_[_]` instead, you use `*[_]` instead of `*`. 

The only use case for the kind projector plugin is to not have to name new types in order to create a new shape. You see how you could create a type alias instead of using the kind projector plugin in the comments of the listing above (they're all called `R`).

Back to the `Users` example we started with, these are three ways of declaring exactly the same thing. It should hopefully clear up any confusion:

```scala
class Users[F[_]: ApplicativeThrowing[*[_], Throwable]]
class Users[F[_]](implicit A: ApplicativeThrowing[F, Throwable])

type AppThrow[F[_]] = ApplicativeThrowing[F, Throwable]
class Users[F[_]: AppThrow]
```

## Referential transparency

The wikipedia text on [Referential transparency](https://en.wikipedia.org/wiki/Referential_transparency) does a good job of explaining this concept. Have a look at the section ["Another example"](https://en.wikipedia.org/wiki/Referential_transparency#Another_example) if the text becomes too abstract at some point.

I like to highlight that in a program where all expressions are referentially transparent, the semantics of the equals sign (`=`) has changed from what it means in an imperative program. It now means "equals" and not "result of", example:

```scala
round1(incrementCounter(3)) >> round2(incrementCounter(3))

// If all functions are referentially transparent, we can refactor to
val increment = incrementCounter(3)
round1(increment) >> round2(increment)
```

The equal sign literally means _equals_. The left hand side is just an alias for the right hand side. They can be used interchangeably. 

## IO Monad

The semantics of `IO[A]` is "a lazily evaluated `A`, the evaluation might fail". The `IO` type from cats-effect has the additional clause "…, and the evaluation might be cancelled", we will return to that later in this text. Note that the IO monad is similar to the `Try` monad: `Try[A]` is "an eagerly evaluated computation of `A` that might have failed". It doesn't have laziness and it doesn't have cancellation, but apart from that they are similar.

The take away here is that it's lazy; the primary reason for using the IO monad is to do lazy evaluation, thunks are another (i.e. `() => A` instead of `A`), but in real world Scala applications we mostly use `IO` <footnote 2 and 3>. Using IO over thunks gives us concurrency and cancellation, and we can build lots of interesting combinators on these primitives. More on that later.

I like to highlight that when all side effects in a program is lazily evaluated, the programmer needs to be explicit about how to compose them, example:

```scala
def run(args: Array[String]): IO[ExitCode] = {
  val context: IO[Counters] = initializeCounters(args)
  val database: IO[Database] = setupDatabase(args)
  
  (context, database).mapN(runLogic).as(ExitCode.Success)
}

def runLogic(counters: Counters, database: Database): IO[Unit] = ???

```

Both `initializeCounters` and `setupDatabase` obviously have side effects. The implementations of these methods typically run as far as they can get without side effects, and then they wrap the remainder in `IO` so that the execution is deferred. This is how we turn functions with side effects into pure functions; it might seem like cheating, but there are benefits to this. 

For example, in the example above, since `setupDatabase` is lazivly evaluated, we can define generic methods for error handling, retries, timeouts, cirtuit breakers, logging etc. and compose them with the value held in `val database`.  We can then focus `setupDatabase` on _only_ that, setting up the database. If it was eagerly evaluated, we'd have to mix all of the generic functionalities just mentioned with the domain logic of setting up the database. Laziness thus gives us increased modularity and reuse, as well as better separation of concerns.

Functional programs is constructed by dividing up execution paths with lazy evaluation, and then gluing them together with `map`, `flatMap` and friends. This is the reason `IO` and `Monad` is A Big Thing™. It's not that they are advanced interfaces per se—they're not—it's just that they are the interfaces that captures laziness (`IO.apply`) and glue (`Monad.{flatMap, map, …}`).

We'll leave the IO monad at that. If you're not entirely sold by the laziness argument, I encourage you to have a look at John Hughes' paper "Why functional programming matteres". You will also hopefully appreciate the power of laziness and pure functions as you go through the Practival FP in Scala book. Finally, the [official documentation](https://typelevel.org/cats-effect/datatypes/io.html) to the IO monad is pretty good (and long). It demonstrates how to build retries, timeouts and many more combinators with the IO monad. You will get far by just remembering that `IO[A]` is a lazily computed `A` though.

#### A short note on Monads in general

Monads are commonly defined with an interface containing two methods

```scala
def pure[A](a: A): F[A]
def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

// we also have map, it's defined in terms of the methods above
def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(f.andThen(pure))
```

There are two useful metaphors that we use when thinking about monads. Try to see how they fit with the definitions above:

* A monad is a container. `map` changes the contents of the container and `flatMap` changes an existing container based on its contents. How does this fit with how you think about `Option`, `Try`,  and `List`?
* A monad is a computational context. `map` changes the result of a computation and `flatMap` adds the next step to a computation based on the result of the previous step. This metaphor fits with how we think about `IO` and `Future`.

The sheer number of monad tutorials on the Internet speaks to that this is a topic lots of programmers struggle with. My advice to you is to not over-complicate it in the beginning, if you understood the text above, then you're more than set for the Practical FP in Scala book. Be as concerned with category theory as you are with graph theory or group theory. They are all awesome and you just need to prioritize your time; a few choose _now_, many choose some time, and some choose never.

If you want to dive deeper into monads, I highly recommend the paper "What we talk about when we talk about monads", see Further reading. 

<footnote 2>: The [Red Book] sticks with functions as a means to achieve laziness for almost the entire book. The `IO` monad isn't introduced until the last part of the book. 

<footnote 3>: Functions allocates stack frames and composing enough functions together inevitably lead to `StackOverflowError`. The IO monad uses Tricks to avoid this (it allocates objects on the heap instead), and this is one of the reasons for why we use it.

## Further reading

If the book and this text is not enough, then you might be interested in these articles:

* Kinds of types in Scala (3-part series): https://kubuszok.com/2018/kinds-of-types-in-scala-part-1/
* What we talk about when we talk about monads: https://arxiv.org/pdf/1803.10195.pdf
* Why functional programming matters: https://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf
* [Haskell wiki/Monads as computation](https://wiki.haskell.org/Monads_as_computation)
* [Hadkell wiki/Monads as containers](http://wiki.haskell.org/Monads_as_containers)

# Chapter 2

## Strongly typed functions

This chapter makes a strong argument for the combination of newtypes and type refinements as the mechanism for storing data in your programs. The primary argument for type refinements is that it captures validation at the type level and lets us depend on and reason about that validation on all use sites. The argument for newtypes is that two data types might have the same validation, but different semantics, we can eliminate a class of bugs by capturing data semantics at the type level as well. For example having two types `Username` and `Password` encapsulating a `String Refined NonEmpty`, instead of two `String Refined NonEmpty`.

A common question is what do you do when you eventually interface with an external API that takes `String`, `Int` and as input, or return those as output, when all you have is `Username`  and `WebserverPort`. Newtypes and refined types will widen into `String` automatically (given the magic imports), so calling such APIs is in general not a problem. For example:

```scala
import eu.timepit.refined.types.net.PortNumber
import eu.timepit.refined.auto._

@newtype case class WebserverPort(toInt: Int Refined PortNumber)
println(Math.max(WebserverPort(80), 0))
// prints 80
```

If you're interfacing with an API that outputs a `String` you will have to validate it and wrap in it the proper type of course. You should _always_ validate wide types such as `String` and `Int` into the refined types. All refined types typically have a companion object extending `RefinedTypeOps`, this gives you an API for running the validation at runtime, it also lets you skip it and lie:

```scala
@ val a: Either[String,PosInt] = PosInt.from(-1)
a: Either[String, PosInt] = Left("Predicate failed: (-1 > 0).")

@ val a: PosInt = PosInt.unsafeFrom(-1)
a: PosInt = -1
```

You should follow this pattern for your own refined types as well—add a companion object and extend `RefinedTypeOps`.

## Effectful computations, `F[_]`

If you've been exposed to code bases written in the tagless final style, dicussed in Chapter 3, you've probably seen `F[_]` a lot. Having a good understanding of [higher kinded types](#higher-kinded-types), you know that it's just a type parameter like any other `A`, just with a restriction on the shape of the type—it needs a hole. We encounter this notation in Chapter 2 as well.

Let's start with a closer look on effects and side-effects, and then look at what abstracting effects gives us. 

Debashish [2] describes effects as "additional structure to a computation". We say that an effectful function `A => F[B]` is a function from `A` to `B` that might contain "non-B-specific-stuff" in `F`. The fact that the return type is `F[B]` and not just `B` is what makes the function "effectful". For example

```scala
def productTry[A, B](a: A, b: B): Try[(A, B)]
```

In the example above we add the _effect of failing_ in `productTry` by using `Try` as `F`. The computation is converting an `A` and a `B` into `(A, B)`, the additional structure in a possible failure and it's captured in the wrapping type `Try`.

### Reasoning with abstract effects

TBD

### Kleisli

You'll see the `Kleisli` type in some of the examples towards the end of the chapter. Just like the type `Function1[A, B]` is the same as `A => B`, a `Kleisli[F, A, B]` is the same as `A => F[B]`. The only reason to prefer a `Kleisli[F, A, B]` over `A => F[B]` is that you get access to more library methods that works with this particular shape of functions. For example, you can compose a `Kleisli[F, A, B]` with a `Kleisli[F, B, C]` and get a `Kleisli[F, A, C]`. That is not so straight forward if you just have `A => F[B]` and `B => F[C]` .




## Applicative and monadic composition,  `*>` , `>>` 

The Cats [Glossary](https://typelevel.org/cats/nomenclature.html) page is helpful when encoutering weird syntax. The ones that are used in the book are covered here:

Both `*>` and `>>` are combinators that combines two values and discards the left one. Examples

```
scala> (IO(println("foo")) *> IO(42)).unsafeRunSync
foo
res0: Int = 42
scala> (IO(println("foo")) >> IO(42)).unsafeRunSync
foo
res1: Int = 42
```

The difference lies in the semantics of the composition. Remember that cats provides two type classes for composing two effectful computations: `Applicative` and `Monad`, the syntax `*>` and `>>` is applicative and monadic composition respectively.  `*>` is an alias for `productRight` and `>>` is an alias for `flatMap`. Here are the method definitions:

```
def productR[A, B](fa: F[A])(fb: F[B]): F[B]
def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
```

We think of `productR` as calculating the product, `F[(A, B)]`, and then map that to the right side. Note that there are two valid implementations of `productR`. For the sake of simplicity, let's assume we have a `Monad` instance, then we can implement `productR` in one of two ways <footnote 1>:

```
def productR[A, B](fa: F[A])(fb: F[B]): F[B] = flatMap(fa)(_ => fb)
def productR[A, B](fa: F[A])(fb: F[B]): F[B] = flatMap(fb)(b => fa.map(_ => b))
```

So either evaluate `F[A]` then `F[B]`, or vice versa. Note that just evaluating `F[B]` is not allowed, we need to calculate the product `F[(A, B)]`. We say that applicative composition expresses composition of _independent_ effects. The fact that there is two valid implemententations captures that the effects are in fact independent. What about `.flatMap`? 

```
def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
```

The only way we can get a `F[B]` is to run the function `f`, but in order to run it we need an `A` so we need to evaluate `F[A]` first. Thus we arrive at the key difference between applicative and monadic composition:

> Applicative composition composes two _independent_ values. You may not assume that one is evaluate before the other, but you may assume that it is sequential and deterministic. Monadic composition composes two _dependent_ values and it's encoded in the definition of `flatMap`.

<footnote 1>: We can implement `productR` in terms of `ap` from `Applicative` also. See the [`Apply`](https://github.com/typelevel/cats/blob/v2.2.0-M1/core/src/main/scala/cats/Apply.scala#L74) class.

## Sequential vs concurrent state

TBD

# References

[1] https://www.microsoft.com/en-us/research/wp-content/uploads/1997/01/multi.pdf
[2] [Functional and Reactive domain modelling](https://www.amazon.com/Functional-Reactive-Domain-Modeling-Debasish/dp/1617292249)





[Red Book]: https://www.amazon.com/Functional-Programming-Scala-Paul-Chiusano/dp/1617290653