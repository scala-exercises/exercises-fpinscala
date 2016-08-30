package fpinscalalib

import java.util.concurrent.ExecutorService

import fpinscalalib.customlib.functionalparallelism.Par
import fpinscalalib.customlib.functionalparallelism.Par._
import org.scalatest.{FlatSpec, Matchers}
import java.util.concurrent.Executors

/** @param name purely_functional_parallelism
  */
object FunctionalParallelismSection extends FlatSpec with Matchers with org.scalaexercises.definitions.Section {
  /**
    * = A data type for parallel computations =
    *
    * Our main objective in this section is to be able to "create parallel computations". What does that mean exactly?
    * Let’s try to refine this into something we can implement by examining a simple, parallelizable computation—summing
    * a list of integers. The usual left fold for this would be as follows:
    *
    * {{{
    *   def sum(ints: Seq[Int]): Int =
    *     ints.foldLeft(0)((a,b) => a + b)
    * }}}
    *
    * Instead of folding sequentially, we could use a divide-and-conquer algorithm; see the following listing.
    *
    * {{{
    *   def sum(ints: IndexedSeq[Int]): Int =
    *     if (ints.size <= 1)
    *       ints.headOption getOrElse 0
    *     else {
    *       val (l, r) = ints.splitAt(ints.length / 2)
    *       sum(l) + sum(r)
    *     }
    * }}}
    *
    * Using `IndexedSeq[Int]` instead of `Seq[Int]` gives us access to an efficient `splitAt` method for dividing the
    * collection into two parts at a particular index. After dividing the sequence in half, we recursively sum both
    * halves, and then combine their results. Unlike the `foldLeft`-based implementation, this one can be parallelized
    * - the two halves can be summed in parallel.
    *
    * Looking at the `sum(l) + sum(r)`, which invokes `sum` on the two halves recursively, we can see that any data type
    * we might choose to rep- resent our parallel computations needs to be able to `contain a result`. That result will
    * have some meaningful type (in this case `Int`), and we require some way of extracting this result.
    *
    * For now, let's `invent` a container type for our result, `Par[A]` (for parallel), and legislate the existence of
    * the functions we need:
    *
    * {{{
    *   def unit[A](a: => A): Par[A]
    *
    *   def get[A](a: Par[A]): A
    * }}}
    *
    * `unit` takes an unevaluated `A` and returns a computation that might evaluate it in a separate thread. We call it
    * `unit` because in a sense it creates a unit of parallelism that just wraps a single value. `get` extracts the
    * resulting value from a parallel computation. Let's update our example with our custom data type (and not worrying
    * about the underlying implementation yet):
    *
    * {{{
    *   def sum(ints: IndexedSeq[Int]): Int =
    *     if (ints.size <= 1)
    *       ints headOption getOrElse 0
    *     else {
    *       val (l, r) = ints.splitAt(ints.length / 2)
    *       val sumL: Par[Int] = Par.unit(sum(l))
    *       val sumR: Par[Int] = Par.unit(sum(r))
    *       Par.get(sumL) + Par.get(sumR)
    *     }
    * }}}
    *
    * Note that we've wrapped the two recursive calls to `sum` in calls to `unit`, and we're calling `get` to extract
    * the two results from the two subcomputations. Also, it's important to realize that `unit` simply returns a
    * `Par[Int]` in this case, representing an asynchronous computation. But as soon as we pass that `Par` to `get`, we
    * explicitly wait for it, generating a side effect. So it seems that we want to avoid calling `get`, or at least
    * delay calling it until the very end. We want to be able to combine asynchronous computations without waiting for
    * them to finish.
    *
    * If we don’t call `get`, that implies that our sum function must return a `Par[Int]`. Let's see how we can modify
    * our implementation taking this into account:
    *
    * {{{
    *   def sum(ints: IndexedSeq[Int]): Par[Int] =
    *     if (ints.size <= 1)
    *       Par.unit(ints.headOption getOrElse 0)
    *     else {
    *       val (l, r) = ints.splitAt(ints.length / 2)
    *       Par.map2(sum(l), sum(r))(_ + _)
    *     }
    * }}}
    *
    * `Par.map2` is a new higher-order function for combining the result of two parallel computations. Its signature is
    * as follows:
    *
    * {{{
    *    def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C]
    * }}}
    *
    * = Explicit forking =
    *
    * Is it always the case that we want to evaluate the two arguments to `map2` in parallel? Probably not. Consider
    * this simple hypothetical example:
    *
    * {{{
    *   Par.map2(Par.unit(1), Par.unit(1))(_ + _)
    * }}}
    *
    * In this case, we happen to know that the two computations we’re combining will execute so quickly that there isn’t
    * much point in spawning off a separate logical thread to evaluate them. But our API doesn’t give us any way of
    * providing this sort of information. That is, our current API is very inexplicit about when computations get forked
    * off the main thread. We can do the forking more explicit by inventing a new function
    * (`def fork[A](a: => Par[A]): Par[A]`), which we can take to mean that the given `Par` should be run in a separate
    * logical thread:
    *
    * {{{
    *   def sum(ints: IndexedSeq[Int]): Par[Int] =
    *     if (ints.length <= 1)
    *       Par.unit(ints.headOption getOrElse 0)
    *     else {
    *       val (l, r) = ints.splitAt(ints.length / 2)
    *       Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    *     }
    * }}}
    *
    * If `fork` simply holds on to its unevaluated argument until later, it requires no access to the mechanism for
    * implementing parallelism. It just takes an unevaluated `Par` and “marks” it for concurrent evaluation. Let’s now
    * assume this meaning for `fork`. With this model, `Par` itself doesn’t need to know how to actually implement the
    * parallelism. It’s more a description of a parallel computation that gets interpreted at a later time by something
    * like the `get` function. `Par` is no longer a `container` of a value that we could simply `get` when it becomes
    * available. Now it's more of a first-class `program` that we can `run`. So let’s rename our `get` function to `run`,
    * and dictate that this is where the parallelism actually gets implemented:
    *
    * {{{
    *   def run[A](a: Par[A]): A
    * }}}
    *
    * Because `Par` is now just a pure data structure, `run` has to have some means of implementing the parallelism,
    * whether it spawns new threads, delegates tasks to a thread pool, or uses some other mechanism.
    *
    * = Picking a representation =
    *
    * Just by exploring this simple example and thinking through the consequences of different choices, we’ve sketched
    * out the following API:
    *
    * {{{
    *   // Creates a computation that immediately results in the value a:
    *   def unit[A](a: A): Par[A]
    *
    *   // Combines the results of two parallel computations with a binary function:
    *   def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C]
    *
    *   // Marks a computation for concurrent evaluation by run:
    *   def fork[A](a: => Par[A]): Par[A]
    *
    *   // Wraps the expression a for concurrent evaluation by run:
    *   def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
    *
    *   // Fully evaluates a given Par, spawning parallel computations as requested by fork, and extracting the result:
    *   def run[A](a: Par[A]): A
    * }}}
    *
    * We know run needs to execute asynchronous tasks somehow. We could write our own low-level API, but there’s already
    * a class that we can use in the Java Standard Library, `java.util.concurrent.ExecutorService`. `ExecutorService`
    * lets us submit a `Callable` value (in Scala we’d probably just use a lazy argument to submit) and get back a
    * corresponding `Future` that’s a handle to a computation that’s potentially running in a separate thread. We can
    * obtain a value from a `Future` with its `get` method (which blocks the current thread until the value is available).
    * Let’s try assuming that our `run` function has access to an `ExecutorService` and see if that suggests anything
    * about the representation for `Par`:
    *
    * {{{
    *   def run[A](s: ExecutorService)(a: Par[A]): A
    * }}}
    *
    * The simplest possible model for `Par[A]` might be `ExecutorService => A.` This would obviously make `run` trivial
    * to implement. But it might be nice to defer the decision of how long to wait for a computation, or whether to
    * cancel it, to the caller of `run`. So `Par[A]` becomes `ExecutorService => Future[A]`, and `run` simply returns
    * the `Future`:
    *
    * {{{
    *   type Par[A] = ExecutorService => Future[A]
    *
    *   def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)
    * }}}
    *
    * Let’s start implementing the functions of the API that we’ve developed so far. Now that we have a representation
    * for `Par`, a first crack at it should be straightforward. What follows is a simplistic implementation using the
    * representation of `Par` that we’ve chosen.
    *
    * {{{
    *   object Par {
    *     private case class UnitFuture[A](get: A) extends Future[A] {
    *       def isDone = true
    *       def get(timeout: Long, units: TimeUnit) = get
    *       def isCancelled = false
    *       def cancel(evenIfRunning: Boolean): Boolean = false
    *     }
    *
    *     def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)
    *
    *     def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    *       (es: ExecutorService) => {
    *         val af = a(es)
    *         val bf = b(es)
    *         UnitFuture(f(af.get, bf.get))
    *      }
    *
    *      def fork[A](a: => Par[A]): Par[A] =
    *        es => es.submit(new Callable[A] {
    *          def call = a(es).get
    *        })
    *      }
    * }}}
    *
    * This API already enables a rich set of operations. Here's a simple example: using `lazyUnit`, let's write a
    * function to convert any function `A => B` to one that evaluates its result asynchronously:
    */

  def parAsyncFAssert(res0: String): Unit = {
    def asyncF[A,B](f: A => B): A => Par[B] =
      a => lazyUnit(f(a))

    def asyncIntToString = asyncF((x: Int) => x.toString())
    val executorService = Executors.newFixedThreadPool(2)

    Par.run(executorService)(asyncIntToString(10)).get() shouldBe res0
  }

  /**
    * Suppose we have a `Par[List[Int]]` representing a parallel computation that produces a `List[Int]`, and we’d like
    * to convert this to a `Par[List[Int]]` whose result is sorted (without having to call `run`):
    *
    * {{{
    *   def sortPar(parList: Par[List[Int]]): Par[List[Int]]
    * }}}
    *
    * The only other combinator we have that allows us to manipulate the value of a `Par` in any way is `map2`. So if w
    * passed `parList` to one side of `map2`, we’d be able to gain access to the `List` inside and sort it. And we can
    * pass whatever we want to the other side of `map2`, so let’s just pass a no-op:
    *
    * {{{
    *   def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    *     map2(parList, unit(()))((a, _) => a.sorted)
    * }}}
    *
    * We can generalize this further. We can “lift” any function of type `A => B` to become a function that takes
    * `Par[A]` and returns `Par[B]`; we can map any function over a `Par`:
    *
    * {{{
    *   def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    *     map2(pa, unit(()))((a,_) => f(a))
    * }}}
    *
    * Let's try to implement `sortPar` via `map`. Remember that you can use the underscore notation to write anonymous
    * functions:
    **/
  def parSortParAssert(res0: List[Int] => List[Int]): Unit = {
    def sortPar(parList: Par[List[Int]]) = map(parList)(res0)

    val executorService = Executors.newFixedThreadPool(2)
    val parList = unit(List(1, 3, 2))
    Par.run(executorService)(sortPar(parList)).get() shouldBe List(1, 2, 3)
  }

  /**
    * What else can we implement using our API? Could we map over a list in parallel? Unlike `map2`, which combines two
    * parallel computations, `parMap` (let’s call it) needs to combine N parallel computations. Let’s see how far we ca
    * get implementing parMap in terms of existing combinators:
    *
    * {{{
    *   def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = {
    *     val fbs: List[Par[B]] = ps.map(asyncF(f))
    *     // ...
    *   }
    * }}}
    *
    * Remember, `asyncF` converts an `A => B` to an `A => Par[B`] by forking a parallel computation to produce the
    * result. So we can fork off our N parallel computations pretty easily, but we need some way of collecting their
    * results. Are we stuck? Well, just from inspecting the types, we can see that we need some way of converting our
    * `List[Par[B]]` to the `Par[List[B]]` required by the return type of `parMap`. Let's try to write the function
    * `sequence` that will help us achieve that:
    *
    * {{{
    *   def sequence_simple[A](l: List[Par[A]]): Par[List[A]] =
    *     l.foldRight[Par[List[A]]](unit(List()))((h,t) => map2(h,t)(_ :: _))
    * }}}
    *
    * Once we have `sequence`, we can complete our implementation of `parMap`:
    *
    * {{{
    *   def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    *     val fbs: List[Par[B]] = ps.map(asyncF(f))
    *     sequence(fbs)
    *   }
    * }}}
    *
    * Note that we’ve wrapped our implementation in a call to `fork`. With this implementation, `parMap` will return
    * immediately, even for a huge input list. When we later call run, it will fork a single asynchronous computation
    * which itself spawns N parallel computations, and then waits for these computations to finish,
    * collecting their results into a list.
    *
    * We can also implement `parFilter`, a function which filters elements of a list in parallel:
    *
    * {{{
    *   def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    *     val pars: List[Par[List[A]]] =
    *     l map asyncF((a: A) => if (f(a)) List(a) else List())
    *     map(sequence(pars))(_.flatten)
    *   }
    * }}}
    **/

  def parFilterAssert(res0: List[Int]): Unit = {
    def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
      val pars: List[Par[List[A]]] =
        l map (asyncF((a: A) => if (f(a)) List(a) else List()))
      map(sequence(pars))(_.flatten)
    }

      val filterOp = parFilter(List(1, 2, 3, 4, 5))(_ < 4)
      val executorService = Executors.newCachedThreadPool()
      val result = Par.run(executorService)(filterOp).get()
      result shouldBe res0
  }

   /** = The algebra of an API =
    *
    * Like any design choice, choosing laws has consequences — it places constraints on what the operations can mean,
    * determines what implementation choices are possible, and affects what other properties can be true. Let’s look at
    * an example. We’ll just make up a possible law that seems reasonable. This might be used as a test case if we were
    * writing tests for our library:
    */

  def parLawMappingAssert(res0: Int) = {
    val lawLeftSide = map(unit(1))(_ + 1)
    val executorService = Executors.newFixedThreadPool(2)
    Par.run(executorService)(lawLeftSide).get() shouldBe Par.run(executorService)(unit(res0)).get()
  }

  /**
    * We’re saying that mapping over `unit(1)` with the `_ + 1` function is in some sense equivalent to `unit(2)`.
    * For now, let’s say two `Par` objects are equivalent if for any valid `ExecutorService` argument, their `Future`
    * results have the same value. We can check that this holds for a particular ExecutorService with a function like
    * this:
    */

  def parEqualAssert(res0: Boolean) = {
    def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = p(e).get == p2(e).get

    val lawLeftSide = map(unit(1))(_ + 1)
    val executorService = Executors.newFixedThreadPool(2)
    equal(executorService)(lawLeftSide, unit(2)) shouldBe res0
  }

  /**
    * The preceding could be generalized this way:
    *
    * {{{
    *   map(unit(x))(f) == unit(f(x))
    * }}}
    *
    * Here we’re saying this should hold for any choice of `x` and `f`, not just `1` and the `_ + 1` function. This
    * places some constraints on our implementation. Our implementation of unit can’t, say, inspect the value it receives
    * and decide to return a parallel computation with a result of `42` when the input is `1` — it can only pass along
    * whatever it receives.
    *
    * Let’s see if we can simplify this law further. We said we wanted this law to hold for any choice of `x` and `f`.
    * Something interesting happens if we substitute the identity function for `f`. We can simplify both sides of the
    * equation and get a new law that’s considerably simpler:
    *
    * {{{
    *   map(unit(x))(f) == unit(f(x))
    *   map(unit(x))(id) == unit(id(x))
    *   map(unit(x))(id) == unit(x)
    *   map(y)(id) == y
    * }}}
    *
    * Let’s consider a stronger property — that `fork` should not affect the result of a parallel computation:
    *
    * {{{
    *   fork(x) == x
    * }}}
    *
    * Surprisingly, this simple property places strong constraints on our implementation of `fork`. After you’ve written
    * down a law like this, take off your implementer hat, put on your debugger hat, and try to break your law. We’re
    * expecting that `fork(x) == x` for all choices of `x`, and any choice of `ExecutorService`. We have a good sense of
    * what `x` could be — it’s some expression making use of `fork`, `unit`, and `map2` (and other combinators derived
    * from these). What about `ExecutorService`? There’s actually a rather subtle problem that will occur in most
    * implementations of `fork`. When using an `ExecutorService` backed by a thread pool of bounded size (see
    * `Executors.newFixedThreadPool`), it’s very easy to run into a deadlock.
    *
    * Can we fix `fork` to work on fixed-size thread pools? Let’s look at a different implementation:
    *
    * {{{
    *   def fork[A](fa: => Par[A]): Par[A] = es => fa(es)
    * }}}
    *
    * This certainly avoids deadlock. The only problem is that we aren’t actually forking a separate logical thread to
    * evaluate `fa`. So `fork(hugeComputation)(es)` for some `ExecutorService` `es`, would run `hugeComputation` in the
    * main thread, which is exactly what we wanted to avoid by calling `fork`. This is still a useful combinator,
    * though, since it lets us delay instantiation of a computation until it’s actually needed. Let’s give it a name,
    * `delay`:
    *
    * {{{
    *   def delay[A](fa: => Par[A]): Par[A] = es => fa(es)
    * }}}
    *
    * But we’d really like to be able to run arbitrary computations over fixed-size thread pools. In order to do that,
    * we’ll need to pick a different representation of `Par`.
    */
}
