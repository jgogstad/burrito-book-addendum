# Addendum for Practical FP in Scala

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->


- [Introduction](#introduction)
- [Prequisites and Chapter 1](#prequisites-and-chapter-1)
  - [Type class recap](#type-class-recap)
  - [Type class coherence](#type-class-coherence)
  - [Higher kinded types](#higher-kinded-types)
    - [Type class revisited](#type-class-revisited)
  - [Kind projector](#kind-projector)
  - [Referential transparency](#referential-transparency)
  - [IO Monad](#io-monad)
  - [Further reading](#further-reading)
- [Chapter 2](#chapter-2)
  - [Strongly typed functions](#strongly-typed-functions)
  - [Effectful computations, `F[_]`](#effectful-computations-f_)
    - [Reasoning with abstract effects](#reasoning-with-abstract-effects)
    - [Kleisli](#kleisli)
  - [Applicative and monadic composition,  `*>` , `>>`](#applicative-and-monadic-composition----)
  - [State and Ref](#state-and-ref)
  - [Error handling](#error-handling)
  - [Further reading](#further-reading-1)
- [Chapter 3 & 4 Tagless final encoding](#chapter-3--4-tagless-final-encoding)
  - [Toy example](#toy-example)
  - [Real world examples, F[_]](#real-world-examples-f_)
    - [Expressing intent](#expressing-intent)
  - [Conclusion](#conclusion)
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

You'll recognize kinds as the mechanism we use to abstract over types. For instance, in the definition `class List[A]` we're restricting the _shape_ of the type parameter to `List` to that of `_` (or `A`). You can pass it anything from the top row in the table above, but not any rows below because they don't belong to the correct kind; `List[IO[HttpResponse]]` compiles, but `List[IO[Option]]` doesn't.

The shapes in the bottom two rows are examples of what we refer to as "Higher kinded types". They are characterized by taking not a value type (e.g. `A`) as a type parameter, but another kind. What is the use of this? It lets us create interfaces that abstracts over other kinds, think ad-hoc polymorphism, example:

```scala
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}
```

Remember that that with ad-hoc polymorphism we take the implementing type as a type parameter. The interface above defines a `map` method, and it can only be implemented for types that has a hole. For example, `List`, `Option` and `IO`, but not `Elephant`, `Either` or `Monad`. We use the type class encoding for polymorphism here because there is no alternative in Scala! That is, there is no way we can create an interface and have a restriction that subtypes of it must have a hole. In languages that doesn't support higher kinded types, this kind of abstraction is not possible to express.

Here's an exercise for you: open a REPL and define a function with a type parameter, play around with what types you can pass to it (I use [Ammonite](https://ammonite.io) below). Try out the types in the table above:

```scala
@ def foo[F[_,_]]: String = "foo"
defined function foo

@ foo[Either]
res2: String = "foo"

@ foo[List]
Compilation Failed
```

### Type class revisited

Now we're ready to revisit our type class definition. A type class is a parameterized trait `I` where the _shape_ of the parameter constrains the shape of the implementing type.

For example, to restrict an interface to only be implementable by types with one hole, we would define our interface as

```scala
trait MyInterface[F[_]] {

}
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

The equal sign literally means _equals_. The left hand side is just an alias for the right hand side. They can be used interchangeably. Runar Bjarnason puts it this way [4]:

> “Referential transparency forces the invariant that everything a function does is represented by the value that it returns, according to the result type of the function. This constraint enables a simple and natural mode of reasoning about program evaluation called the substitution model. When expressions are referentially transparent, we can imagine that computation proceeds much like we’d solve an algebraic equation. We fully expand every part of an expression, replacing all variables with their referents, and then reduce it to its simplest form. At each step we replace a term with an equivalent one; computation proceeds by substituting equals for equals. In other words, RT enables equational reasoning about programs.”
>

## IO Monad

The semantics of `IO[A]` is "a lazily evaluated `A`, the evaluation might fail or be cancelled". Note that apart from the cancellation part, the IO monad is similar to the `Try` monad: `Try[A]` is "an eagerly evaluated computation of `A` that might have failed".

The _purpose_ of the IO monad is to turn side effects into values, values which can then be composed later on. The mechanism it uses for this is laziness. When you wrap a code block or a method invocation in `IO`, it's lazily evaluated.

When all side effects in a program are values, the programmer needs to be explicit about how to compose them, for example:

```scala
def run(args: Array[String]): IO[ExitCode] = {
  val context: IO[Counters] = initializeCounters(args)
  val database: IO[Database] = setupDatabase(args)
  
  (context, database).mapN(runLogic).as(ExitCode.Success)
}

def runLogic(counters: Counters, database: Database): IO[Unit] = ???

```

Both `initializeCounters` and `setupDatabase` obviously have side effects. The implementations of these methods typically run as far as they can get without side effects, and then they wrap the remainder in `IO` so that the execution is deferred. This is how we turn functions with side effects into pure functions. In the example above we use `mapN` to combine them, this communicates that the side effects are independent, more about _applicative and monadic composition_ later on.

Apart from being explicit about what side effects are dependent and not, here are some other things we can model with cats-effect and `IO`:

* Declarative and easy-to-reason about concurrency and parallelism
* Resource safety across concurrent and parallel boundaries
* Primitivies that lets us easily build combinators like circuit breakers, timeouts, retry logic etc.
* Green threads, also known as fibers

By treating side-effects as values we get increased modularity and reuse, better separation of concerns and easier to reason about programs, particularly in the case of concurrency and parallelism.

The [official documentation](https://typelevel.org/cats-effect/datatypes/io.html) to the IO monad is pretty good (and long). It demonstrates how to build retries, timeouts and many more combinators with the IO monad. You will get far by just remembering that `IO[A]` is a lazily computed `A` though, accompanied by a side-effect.

#### A short note on Monads in general

Functional programs are constructed by dividing up execution paths with lazy evaluation, and then gluing them together with `map`, `flatMap` and friends. This is the reason `IO` and `Monad` is A Big Thing™. It's not that they are advanced interfaces per se—they're not—it's just that they are the interfaces that captures laziness (`IO.apply`) and glue (`Monad.{flatMap, map, …}`).

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

If you've been exposed to code bases written in the tagless final style, dicussed in Chapter 3, you've probably seen `F[_]` a lot. Having a good understanding of [higher kinded types](#higher-kinded-types), you now know that it's just a type parameter like any other `A`, just with a restriction on the shape of the type—it needs a hole. We encounter this notation in Chapter 2 as well.

Let's start with a closer look on effects and side-effects, and then look at what abstract effects accomplishes. 

Debashish [2] describes effects as "additional structure to a computation". We say that an effectful function `A => F[B]` is a function from `A` to `B` that might contain "non-B-specific-stuff" in `F`. The fact that the return type is `F[B]` and not just `B` is what makes the function "effectful". For example

```scala
def divide(a: Int, b: Int): Try[Int]
```

In the example above we add the _effect of failing_ in `productTry` by using `Try` as `F`. The computation is converting two `Int`s into a single `Int`; the additional structure in a possible failure and it's captured in the wrapping type `Try`. With the metaphore "F is a computational context" in mind, then:

* `IO` is the effect of lazy evaluation that can be cancelled and run concurrently
* `Option` is the effect of missing value
* What about `List`? It has a hole, but it is usually not thought of as an effect. It fits better with the metaphor "F is a container". You can say that it's an effect that adds the structure of repetition though.

What is a _side effect_ then? Debashish [2] defines it as

> A side effect is something that’s not within the control of the function that you implement. If you’re manipulating the filesystem, or using databases or any other external resource from within your function, you are creating side effects.

We think of side effects as state changes external to the entity in question. So a side effect from the perspective a function can for example be mutating state outside of the function. A side effect of a program, likewise, is changing state of another system (for example file system) external to the program.

### Reasoning with abstract effects

Take the following function definition

```scala
def product(a: Try[Int], b: Try[Int]): Try[(Int, Int)]
```

what are valid implementations of this method? One expect something along the lines of:

```scala
def product(a: Try[Int], b: Try[Int]): Try[(Int, Int)] =
  a.flatMap(valueA => b.map(a -> _))
```

But there are others as well:

```scala
def product(a: Try[Int], b: Try[Int]): Try[(Int, Int)] =
  Try(a.get -> b.get)

def product(a: Try[Int], b: Try[Int]): Try[(Int, Int)] =
  Try {
    val valueA = a.get
    if (valueA < 0) Failure("Negative numbers not supported")
    else Try(valueA, b.get)
  }
```

In fact there are infinitely many. If we _code against the API and not the implementation_ we can be more specific about what APIs are actually in use, this should sound familiar to anyone with a Java/C#/C++ background. Let's abstract `Try` into an abstract type `F[_]` and declare what interfaces were using:

```scala
import cats.syntax.all._

def product[F[_]: Monad](a: F[Int], b: F[Int]): F[(Int, Int)] =
  a.flatMap(valueA => b.map(valueA -> _))
```

How do you invoke it? You pass the type parameter the same way as you would to any other parameterized type or `def`: 

```scala
val a: Try[Int] = ???
val b: Try[Int] = ???
product[Try](a, b)
```

Remember that scala's _context bounds_ is syntactic sugar for an implicit parameter list, so the following two defs are equal <footnote 1>:

```scala
def product[F[_]: Monad](a: F[Int], b: F[Int]): F[(Int, Int)] = ???
def product[F[_]](implicit M: Monad[F])(a: F[Int], b: F[Int]): F[(Int, Int)] = ???
```

In order to invoke with `Try` as we do above, we would need to bring an implicit into scope of type `Monad[Try]`. You can import this from cats, either with the catch-all `import cats.implicits._` or the specific `import cats.instances.try_`.

So what did this achieve? We reduced the number of valid implementations. Many of the implementations above used `Try` specific APIs, and those no longer compiles. If we settle for the `flatMap` and `map` implementation above, we can abstract over `Int` also, arriving at:

```scala
def product[F[_]: Monad, A, B](a: F[A], b: F[B]): F[(A, B)] =
  a.flatMap(valueA => b.map(a -> _))
```

In fact, this is the _only_ valid implementation of the def's signature. By _constraining_ ourselves to only working with the Monad API and knowing nothing about the types `A` and `B` we eliminate many implementations of the method signature, and clients can easier reason about what it does without being exposed to the details of the implementation. To quote Runar Bjarnason [1]:

> Constraints liberate, liberties constrain

This example is kind of extreme (but not uncommon); in general we aim to know just-enough about our inputs and maximize what clients can infer about the implementation.

### Kleisli

You see the `Kleisli` type in some of the examples towards the end of the chapter. Just like the type `Function1[A, B]` is the same as `A => B`, a `Kleisli[F, A, B]` is the same as `A => F[B]`. The only reason to prefer a `Kleisli[F, A, B]` over `A => F[B]` is that you get access to more library methods that works with this particular shape of functions. For example, you can compose a `Kleisli[F, A, B]` with a `Kleisli[F, B, C]` and get a `Kleisli[F, A, C]`. That is not so straight forward if you just have `A => F[B]` and `B => F[C]` . Kleisli's are invoked in the same way as `Function[A,B]`: `someKleisli(42)`.



<footnote 1>: I like how the semantics of the colon of a context bound is similar to the colon in type annotations. For example in `val a: Int` the colon denotes "is-a" relationsihp; so `a`-is-an-int. Similarly in `F[_]: Monad` can be read as `F`-is-a-monad.


## Applicative and monadic composition,  `*>` , `>>` 

The Cats [Glossary](https://typelevel.org/cats/nomenclature.html) page is helpful when encountering weird syntax. The ones that are used in the book are covered here:

Both `*>` and `>>` are combinators that combines two values and discards the left one. Examples

```scala
scala> (IO(println("foo")) *> IO(42)).unsafeRunSync
foo
res0: Int = 42
scala> (IO(println("foo")) >> IO(42)).unsafeRunSync
foo
res1: Int = 42
```

The difference lies in the semantics of the composition. Remember that cats provides two type classes for composing two effectful computations: `Applicative` and `Monad`, the syntax `*>` and `>>` is applicative and monadic composition respectively.  `*>` is an alias for `productRight` and `>>` is an alias for `flatMap`. Here are the method definitions:

```scala
def productR[A, B](fa: F[A])(fb: F[B]): F[B]
def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
```

We think of `productR` as calculating the product, `F[(A, B)]`, and then map that to the right side. Note that there are two valid implementations of `productR`. For the sake of simplicity, let's assume we have a `Monad` instance, then we can implement `productR` in one of two ways <footnote 1>:

```scala
def productR[A, B](fa: F[A])(fb: F[B]): F[B] = flatMap(fa)(_ => fb)
def productR[A, B](fa: F[A])(fb: F[B]): F[B] = flatMap(fb)(b => fa.map(_ => b))
```

So either evaluate `F[A]` then `F[B]`, or vice versa. Note that just evaluating `F[B]` is not allowed, we need to calculate the product `F[(A, B)]`. We say that applicative composition expresses composition of _independent_ effects. The fact that there is two valid implemententations captures that the effects are in fact independent. What about `.flatMap`? 

```scala
def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
```

The only way we can get a `F[B]` is to run the function `f`, but in order to run it we need an `A` so we need to evaluate `F[A]` first. Thus we arrive at the key difference between applicative and monadic composition:

> Applicative composition composes two _independent_ values. You may not assume that one is evaluate before the other, but you may assume that it is sequential and deterministic. Monadic composition composes two _dependent_ values and it's encoded in the definition of `flatMap`.

## State and Ref

The author presents `State` as the sequential counterpart to `Ref`. This is the first and last time you will encounter `State` in this book, and I would say it's one of the lesser used monads. If the sequential part wasn't entirely clear, we're revisiting that in this section. That said, `Ref` is the more commonly used data structure for handling state so feel free to skip ahead.

I like to use the "a monad is a container" metaphor when thinking about `State`. The `State` container contains a value as usual, and an additional "state" structure which is also a value. The `map` and `flatMap` functions behave like they do for other types where this metapahor fits (`Option`, `Try`, …), `map` lets you change the value and `flatMap` lets you change the entire container. In addition, there are methods for inspecting and changing the state part of the monad.

The point with "sequential state" is just to contrast it with state that would be accessible from multiple places at once. A cache would be an example of state that is not sequential—multiple threads could access it at the same time. Let's take an example of "sequential state", this time a copy from the Cats documentation on [State](https://typelevel.org/cats/datatypes/state.html) with some very minor modifications:

```scala
final case class Robot(
  id: Long,
  sentient: Boolean,
  name: String,
  model: String)

object Robot {
  private val rng = new scala.util.Random(0L)
  
  def createRobot(): Robot = {
    val id = rng.nextLong()
    val sentient = rng.nextBoolean()
    val isCatherine = rng.nextBoolean()
    val name = if (isCatherine) "Catherine" else "Carlos"
    val isReplicant = rng.nextBoolean()
    val model = if (isReplicant) "replicant" else "borg"
    Robot(id, sentient, name, model)
  }
}
```

The random number generator `rng` is accessed sequentially—one invocation after another. Note that `rng.nextBoolean` is not referentially transparent. We can't refactor multiple identical expression into a single expression without changing semantics. We can make it referentially transparent with the `State` monad; the cats documentation already does a brilliant job at describing how, see the section [Cleaning it up with State](https://typelevel.org/cats/datatypes/state.html#cleaning-it-up-with-state).

In summary, State is useful whenever you find yourself in need of a locally scoped `var`. In the example above, the `var` is used internally in `scala.util.Random` (it is actually not locally scoped, but we assume that thread safety is handled internally in `scala.util.Random`). In such scenarios, you can use `State` to carry your state and avoid the `var`.

Is `var` always bad? No, it isn't. Referential transparency is not an end goal in itself, it just a means to achieve code that is easy to reason about, code that survives handoffs between multiple maintainers, and code that is verifiable by the compiler. I think for this reason, `State` is not as ubiqutous as many other monads, in fact, I don't believe it's much used and it's the last time you'll encounter it in this book. If you want some social evidence that `var` is OK, find your favorite purely functional library on github and do a code search, you will most likely find plenty.

## Error handling

The error handling chapter demonstrates `MonadError` and `ApplicativeError` for error handling, and you also meet our old friend the [kind projector](#kind-projector) plugin. Note that the idiomatic way of working with these types, is just to define a type alias, like he does:

```scala
type ApThrow[F[_]] = ApplicativeError[F, Throwable]
```

The kind projector is cool and all, but if we can have less typing and less abstractions we choose that. There are use cases for it, like the classy prisms example that ends this chapter, but for `ApplicativeError` with `Throwable` as the error type, a type alias is just fine.

The last example of this chapter is probably premature if you haven't worked with http4s and that's fine. It demonstrates a technique where we can be more specific about what ways our code can fail.

<footnote 1>: We can implement `productR` in terms of `ap` from `Applicative` also. See the [`Apply`](https://github.com/typelevel/cats/blob/v2.2.0-M1/core/src/main/scala/cats/Apply.scala#L74) class.

## Further reading

* In the context of effectful computations; the term _parametricity_ was coined by Philip Wadler to describe the increase in reasoning you obtain by parametric polymorphism, you can read more about it in the paper "Theorems for free!" [4].

# Chapter 3 & 4 Tagless final encoding

> Sometimes I ask myself, why are we doing all this? What's the purpose?
>
> – Colleague

These chapters introduces the _tagless final encoding_ in Scala and gives an example of how this way of structuring program code is applied to the book's running example.

One way to create programs, in any programming language, is to first create a DSL to express them with and then "run" the DSL. The pieces of the DSL, the "atoms" or syntax if you will—restricts what kind of logic that can be expressed in it; with tight and precise syntax follows less invalid programs.

The "tagless final" approach to DSLs is a bit peculiar if you come from a Java, C# or other high level imperative languages. We're going to explain tagless final by first looking at an _initial encoding_ in Java, and then contrast that with the tagless final encoding.

Here is an example of a Java DSL that constructs a program that transfers data between files (expressed in [Apache Camel](https://camel.apache.org)): 

```java
from("file:src/data?noop=true")
  .choice()
    .when(xpath("/person/city = 'London'"))
      .to("file:target/messages/uk")
    .otherwise()
      .to("file:target/messages/others");
```

The _syntax_ of the DSL is `from`, `choice` , `when` etc; more specifically, it's the types that these functions return. After invoking `from(…)` the returned object's type constrains the operations you might continue with; the programmer constructs programs using a "fluent API", always calling methods on the last returned value. The result from running the code is an object structure which needs to be explicitly interpreted and run by Apache Camel <footnote 1>.

This is an example of an _initial encoding_. The DSL builds up an object graph and the interpreter needs to match on the types in the graph to execute it. It suffers from what is known as the _expression problem_ [7]. It is impossible to add operations to the DSL without recompiling the entire DSL (Apache Camel in this example).

Tagless final is a pattern that is used to express custom DSLs for your domain and interpreters for them. The resulting DSL doesn't suffer from the expression problem—it's easy to add operations to it without recompiling existing interpreters. Tagless final is not a library, it's just a way to structure a DSL, fluent APIs are another.

By expressing programs with tagless final we aim to achieve the following, try to see if you can identify how items in this list are achieved when reading ahead:

1. Only valid programs can be expressed

2. The DSL can be wrapped up in a JAR, and still extended by a client—meaning, adding more operations to it.

3. We can easily "run" the DSL in-memory or with side effects

4. The type of programs expresses how the DSL is used to clients

Let's start with a toy example from one of the original papers, expressed in Scala.

<footnote 1>: Is this functional programming in Java? Yes it is!

## Toy example

> A popular way to define and implement a language is to embed it in another. Embedding means to represent terms and values of the object language as terms and values in the metalanguage, so as to interpret the former in the latter [6]

The "language" here is the _Domain Specific Language_ (DSL), the metalanguage is Scala. The toy example we're expressing is translated from [8]. The DSL is expressed as a trait with a type parameter, all returned values are of the parameterized type:

```scala
  trait FExp[A] {
    def lit(value: Int): A

    def add(lhs: A, rhs: A): A

    def sub(lhs: A, rhs: A): A

    def mul(lhs: A, rhs: A): A
  }
```

"FExp" is the name of our DSL, it contains the _syntax_ of our language. The book uses the term "Algebra" for this type, the intention is to express that it contains the syntax of which we construct our programs. We usually take the term "Algebra" to mean

> “a collection of functions operating over some data type(s), along with a set of laws specifying relationships between these functions”
>
> Excerpt From: Paul Chiusano Rúnar Bjarnason. “Functional Programming in Scala” [9].

Algebras without laws doesn't really make sense, but the term has stuck in the Scala community so we'll use it here as well <footnote 1>.

 Programs are expressed by combing the syntax, for example:

```scala
def program[A](dsl: FExp[A]): A = {
  val addTwo: A   = dsl.add(dsl.lit(1), dsl.lit(2))
  val negate: A   = dsl.negate(addTwo)
  
  dsl.add(dsl.lit(8), negate)
}
```

What is the result of running this program? It depends on the choice of `A` and the implementation of `FExp[A]`. We say that we choose the _semantic domain_ of the DSL by choosing a type for `A`, and we call the implementation of the trait for the _interpreter_ for that `A`.

For the example above we could say that the we have `String` semantics and a `String` interpreter. It would format the expression to a pretty `String`:

```scala
class StringInterpreter extends FExp[String] {
  def lit(value: Int): String
  def add(lhs: String, rhs: String): String
  …
}
```

Or, we could choose `Int` as the domain and have integer addition, subtraction etc:

```scala
class IntInterpreter FExp[Int] {
  def lit(value: Int): Int
  def add(lhs: Int, rhs: Int): Int
  …
}
```

We can extend the language by adding more algebras and parameterize them on the same `A`, for instance

```scala
trait Logarithms[A] {
  def logN(a: A, base: Int): A
}
```

Programs can take both algebras as input and combine them since they are parameterized on the same `A`:

```scala
class SomeProgram[A](
  fexp: FExp[A],
  logs: Logarithm[A]
) {
  def logSquare(i: Int): A = {
    val square = fext.mul(fexp.lit(i), fexp.lit(i))
    logs.logN(square, 2)
  }
}
```

So even if `FExp[A]` was packaged up in a library, we can add syntax to the DSL without recompilinig `FExp`.

_Exercise 1:_ 

* Create an initial coding called `IExp` for `FExp[A]` and write an interpreter for it.
* Why is it not possible to extend this DSL?

_Solution 1_: See [eli-jordan/tagless-final-jam](https://github.com/eli-jordan/tagless-final-jam/blob/master/arithmetic/src/main/scala/arithmetic/Arithmetic.scala)

_Exercise 2:_ Express laws for `FExp[A]`. 

    a) The algebra should be associative for all operators (that is `4 + (5 + 6) == (4 + 5) + 6 `)

    b) Express the law of unitality for `add`: `4 + 0 == 0 + 4`. Does it hold for both interpreters?

<footnote 1> : It is possible to express laws for some of these algebras, and very much for this toy example. Have a look at [discipline](https://github.com/typelevel/discipline) for a good library to express laws with.

## Real world examples, F[_]

The book introduces tagless final with the following algebra

```scala
trait Items[F[_]] {
  def getAll: F[List[Item]] 
  def add(item: Item): F[Unit]
}
```

You see the only difference from the toy example is the shape of the type parameter. It's now a type constructor and not a value type. The point here is that we abstract over the semantic domain, what shape it has isn't really important. In practice it's usually `F[_]` and then we call it the _effect_ and not semantic domain. Can you think of some valid effects and interpreters? 

Here's a couple:

* `ItemsLive[IO]`: Interpreter for a semantic domain that captures side effects
* `ItemsInMemory[State[List[Item], *]]`: Semantic domain keeps state in memory

For other algebras you can imagine using the usual suspects `Try`, `Option` and `Id` to capture failure, absence and no-effects respectively.

_Exercise 2:_ Implement `ItemsInMemory[State[List[Item], *]]` and write a program that uses it

_Exercise 3:_ 

    a) Choose an effect for your in-memory interpreter that support errors, but not side effects

    b) (Hard) Can you create an in-memory interpreter that can be used in programs that requires `Concurrent` for `F[_]`? Make a decision whether you want to really have concurrency or not in your tests and create an interpreter accordingly.


### Expressing intent

Let's look at how programs expressed with in tagless final exposes important implementation details in the types. From the book we have this program:

```scala
final class CheckoutProgram[F[_]: Monad](
  paymentClient: PaymentClient[F],
  shoppingCart: ShoppingCart[F],
  orders: Orders[F]
) {

  def checkout(userId: UserId, card: Card): F[OrderId] = for {
    cart      <- shoppingCart.get(userId)
    paymentId <- paymentClient.process(Payment(userId, cart.total, card))
    orderId   <- orders.create(userId, paymentId, cart.items, cart.total)
    _         <- shoppingCart.delete(userId)
  } yield orderId
  
}
```

Ignore the implementation of `checkout` for now, just focus on the type of the program

```scala
final class CheckoutProgram[F[_]: Monad](
  paymentClient: PaymentClient[F],
  shoppingCart: ShoppingCart[F],
  orders: Orders[F]
)
```

From this type we can read that the intention of the original author was to

1. Sequentially compose operations from one or more of the input algebras

Equally important we can reason about what was _not_ the intention of the original author:

* No raising of errors
* No spawning threads or running concurrently
* No blocking IO

How can we be so sure? The only restriction the author put on `F` is that is a `Monad`, thus this program doesn't have any APIs to raise errors, spawn threads or do blocking IO. It can _only_ use the interface defined by `Monad` , and that's only `flatMap`. If the original author intented to do applicative composition, or just use `.map`, she would've required `Applicative` or `Functor` instead. If she intended to do concurrency or raise errors, then `Concurrent` and `MonadThrowing`, and so on.

Note that the underlying algebra's could still have these capabilities, we would have to look at the type of the interpreters to know. 

## Conclusion

We've seen that, as other DSLs, tagless final lets us put restrictions on how to express programs. With these restrictions we can limit the number of expressable programs to only, or close to only, valid programs. 

The approach the book takes to creating programs is sometimes called _algebraic design_ [9], we define algebras and compose them into larger programs—only relying on the laws of the algebras to reason about the programs. Then we decide on a representation of the algebras later on.

Finally we saw that tagless final captures _intent_ about large chunks of code in the program types. When we have our maintainer's hat on, we can see what the original author intended and we don't need to read and undertsand every line of code in order to reason about for instance failures or parallelism.

# References

[1] https://www.microsoft.com/en-us/research/wp-content/uploads/1997/01/multi.pdf

[2] [Functional and Reactive domain modelling](https://www.amazon.com/Functional-Reactive-Domain-Modeling-Debasish/dp/1617292249)

[3] https://www.youtube.com/watch?v=GqmsQeSzMdw

[4] https://ecee.colorado.edu/ecen5533/fall11/reading/free.pdf

[5] https://www.amazon.com/Functional-Programming-Scala-Paul-Chiusano/dp/1617290653

[6] [Finally Tagless, Partially Evaluated](http://okmij.org/ftp/tagless-final/JFP.pdf)

[7] https://homepages.inf.ed.ac.uk/wadler/papers/expression/expression.txt

[8] [Typed tagless final interpreters](http://okmij.org/ftp/tagless-final/course/lecture.pdf)

[9] [The Red Book]( https://www.amazon.com/Functional-Programming-Scala-Paul-Chiusano/dp/1617290653)
