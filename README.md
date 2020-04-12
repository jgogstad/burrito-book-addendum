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
- [Chapter 2](#chapter-2)
  - [Cats syntax: `>>` , `*>`](#cats-syntax---)
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

This is if course not a substitue for learning functional programming fundamentals, but we find that requiring new team members to go through the [Red Book](https://www.amazon.com/Functional-Programming-Scala-Paul-Chiusano/dp/1617290653) then [Scala with Cats](https://underscore.io/books/scala-with-cats/), and then maybe "Practical FP in Scala" is neither realistic nor practical. We strongly believe that everyone can appreciate and be productive in functional code bases without a high up-front investment in understanding the fundamentals.

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

You may wonder how you could easily use implementations encoded with ad-hoc polymorphism. For Scala specifically the answer lies in the implicit mechanism. Please see the cats book for examples. 

Type classes are interfaces expressed with the ad-hoc encoding with the additional constraint that they are _coherent_. 

## Type class coherence

One maybe not so obvious property of ad-hoc polymorphism is that it allows for implementing the same interface multiple times for the same type. For example:

```scala
// Using Scala 2.12 function syntax for traits with a single def
val prettyElephant: PrettyPrint[Elephant] = a => "An elephant named ${a.name}"
val veryPrettyElephant: PrettyPrint[Elephant] = a => "-=- An elephant named ${a.name} -=-"
```

_Coherence_ refers to the situation when

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

The star here is not to be mixed up with the star in the kind projector in the next section. The star here is the formal syntax for kinds, if you want to read more about kinds, you'll encounter this. The "scala shape" column is the encoding Scala uses. It's not the easiest to read!

You'll recognize kinds as the mechanism we use to abstract over types. For instance, in the definition `class List[A]` we're restricting the _shape_ of the type parameter to `List`. You can pass it anything from the top row in the table above, but not any rows below because they don't belong to the correct kind.

Here's an exercise for you: open a REPL and define a function with a type parameter, play around with what types to can pass to it (I use [Ammonite](https://ammonite.io) below). Try out the types in the table above:

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

```
Tuple2[*, Double]        // equivalent to: type R[A] = Tuple2[A, Double]
Either[Int, +*]          // equivalent to: type R[+A] = Either[Int, A]
Function2[-*, Long, +*]  // equivalent to: type R[-A, +B] = Function2[A, Long, B]
EitherT[*[_], Int, *]    // equivalent to: type R[F[_], B] = EitherT[F, Int, B]
```

You place the `*` where you want the new holes to be; the new holes will have shape `_`. If you need to make a hole with shape `_[_]` instead, you use `*[_]` instead of `*`. The only use case for this is to not have to name the new type, as you would've done if you created a type alias (`R` in the example above).

Back to the `Users` example we started with, these are three ways of declaring exactly the same thing. It should hopefully clear up any confusion:

```
class Users[F[_]: ApplicativeThrowing[*[_], Throwable]]
class Users[F[_]](implicit A: ApplicativeThrowing[F, Throwable])

type AppThrow[F[_]] = ApplicativeThrowing[F, Throwable]
class Users[F[_]: AppThrow]
```

## Referential transparency

The wikipedia text on [Referential transparency](https://en.wikipedia.org/wiki/Referential_transparency) does a good job of explaining this concept. Have a look at the section ["Another example"](https://en.wikipedia.org/wiki/Referential_transparency#Another_example) if the text becomes too abstract at some point.

# Chapter 2

## Cats syntax: `>>` , `*>` 

The Cats [Glossay](https://typelevel.org/cats/nomenclature.html) page is helpful when encoutering weird syntax. The ones that are used in the book are covered here:

Both `*>` and `>>` are combinators that combines two values and discards the left one. Examples

```
scala> (IO(println("foo")) *> IO(42)).unsafeRunSync
foo
res0: Int = 42
scala> (IO(println("foo")) >> IO(42)).unsafeRunSync
foo
res1: Int = 42
```

The difference lies in the semantics of the composition. Remember that cats provides two type classes for composing two effectful computations: `Applicative` and `Monad`, the syntax `*>` and `>>` is applicative and monadic composition respectively.  Example:

```
val a: IO[Int] = ???
val b: IO[Int] = ???

// These two are identical <footnote 1>
val result: IO[Int] = (a, b).mapN((_, vb) => vb)
val result: IO[Int] = a *> b

// These two are identical
val result2: IO[Int] = a.flatMap(_ => b)
val result2: IO[Int] = a >> b
```

The key take away is this: Applicative composition, `*>`, composes two independent effectful computations; monadic composition `>>` composes two dependent effectful computations—the first one needs to run, _then_ the second one needs to run. 

If two computations are composed with `*>` it means that they could've been parallelized since they don't depend on one another; that is not the case with `>>`. Also note that there's no need to evaluate the expression `b` in the case of `>>`, it can be (and it is) evaluated lazily.



<footnote 1>: The actual implementation of `*>`, or `productRight`, uses `ap` from Applicative, but it _could've_ used `mapN`. The implementation of `productLeft`, or `<*` does this.

## Sequential vs concurrent state



# References

[1] https://www.microsoft.com/en-us/research/wp-content/uploads/1997/01/multi.pdf
