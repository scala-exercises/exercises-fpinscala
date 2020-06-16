/*
 * Copyright 2016-2020 47 Degrees Open Source <https://www.47deg.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package fpinscalalib.customlib.functionaldatastructures

// The following implementation of the binary tree is provided by Manning as a solution to the multiple implementation
// exercises found in the "Functional Programming in Scala" book. The original code can be found in the following URL:
//
// https://github.com/fpinscala/fpinscala/blob/5bf1138f3aa71ff91babaa99613313fb9ac48b27/answers/src/main/scala/fpinscala/datastructures/Tree.scala

sealed trait Tree[+A]
case class Leaf[A](value: A)                        extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int =
    t match {
      case Leaf(_)      => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }

  /*
  We're using the method `max` that exists on all `Int` values rather than an explicit `if` expression.

  Note how similar the implementation is to `size`. We'll abstract out the common pattern in a later exercise.
   */
  def maximum(t: Tree[Int]): Int =
    t match {
      case Leaf(n)      => n
      case Branch(l, r) => maximum(l) max maximum(r)
    }

  /*
  Again, note how similar the implementation is to `size` and `maximum`.
   */
  def depth[A](t: Tree[A]): Int =
    t match {
      case Leaf(_)      => 0
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    t match {
      case Leaf(a)      => Leaf(f(a))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

  /*
  Like `foldRight` for lists, `fold` receives a "handler" for each of the data constructors of the type, and recursively
  accumulates some value using these handlers. As with `foldRight`, `fold(t)(Leaf(_))(Branch(_,_)) == t`, and we can use
  this function to implement just about any recursive function that would otherwise be defined by pattern matching.
   */
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B =
    t match {
      case Leaf(a)      => f(a)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(a => 1)(1 + _ + _)

  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)(a => a)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(a => 0)((d1, d2) => 1 + (d1 max d2))

  /*
  Note the type annotation required on the expression `Leaf(f(a))`. Without this annotation, we get an error like this:

  type mismatch;
    found   : fpinscala.datastructures.Branch[B]
    required: fpinscala.datastructures.Leaf[B]
       fold(t)(a => Leaf(f(a)))(Branch(_,_))
                                      ^

  This error is an unfortunate consequence of Scala using subtyping to encode algebraic data types. Without the
  annotation, the result type of the fold gets inferred as `Leaf[B]` and it is then expected that the second argument
  to `fold` will return `Leaf[B]`, which it doesn't (it returns `Branch[B]`). Really, we'd prefer Scala to
  infer `Tree[B]` as the result type in both cases. When working with algebraic data types in Scala, it's somewhat
  common to define helper functions that simply call the corresponding data constructors but give the less specific
  result type:

    def leaf[A](a: A): Tree[A] = Leaf(a)
    def branch[A](l: Tree[A], r: Tree[A]): Tree[A] = Branch(l, r)
   */
  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
}
