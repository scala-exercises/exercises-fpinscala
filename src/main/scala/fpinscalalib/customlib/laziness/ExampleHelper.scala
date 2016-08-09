package fpinscalalib.customlib.laziness

object ExampleHelper {
  def if2[A](cond: Boolean, onTrue: () => A, onFalse: () => A): A = if (cond) onTrue() else onFalse()
}
