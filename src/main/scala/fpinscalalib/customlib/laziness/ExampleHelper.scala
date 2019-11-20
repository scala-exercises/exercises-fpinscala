/*
 *  scala-exercises - exercises-fpinscala
 *  Copyright (C) 2015-2019 47 Degrees, LLC. <http://www.47deg.com>
 *
 */

package fpinscalalib.customlib.laziness

object ExampleHelper {
  def if2[A](cond: Boolean, onTrue: () => A, onFalse: () => A): A =
    if (cond) onTrue() else onFalse()
}
