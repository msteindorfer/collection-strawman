package strawman.collection

import scala.{Boolean, Unit}

/** A template trait that contains just the `map`, `flatMap`, `foreach` and `withFilter` methods
  *  of trait `Iterable`.
  *
  * @tparam A Element type (e.g. `Int`)
  * @tparam C Collection type (e.g. `List[Int]`)
  */
class WithFilter[+A, +C](coll: C, iterable: Iterable[A], p: A => Boolean) {

  /** Builds a new collection by applying a function to all elements of the
    *  outer $coll containing this `WithFilter` instance that satisfy predicate `p`.
    *
    *  @param f      the function to apply to each element.
    *  @tparam B     the element type of the returned collection.
    *  @tparam C2    the type of the returned collection.
    *  @param bf     $bfinfo
    *  @return       a new collection of type `C2` resulting from applying
    *                the given function `f` to each element of the outer $coll
    *                that satisfies predicate `p` and collecting the results.
    */
  def map[B, C2](f: A => B)(implicit bf: BuildFrom[C, B, C2]): C2 = {
    val b = bf.newBuilder(coll)
    for (x <- iterable)
      if (p(x)) b += f(x)
    b.result()
  }

  /** Builds a new collection by applying a function to all elements of the
    *  outer $coll containing this `WithFilter` instance that satisfy
    *  predicate `p` and concatenating the results.
    *
    *  @param f      the function to apply to each element.
    *  @tparam B     the element type of the returned collection.
    *  @tparam C2    the type of the returned collection.
    *  @param bf     $bfinfo
    *  @return       a new collection of type `C2` resulting from applying
    *                the given collection-valued function `f` to each element
    *                of the outer $coll that satisfies predicate `p` and
    *                concatenating the results.
    */
  def flatMap[B, C2](f: A => IterableOnce[B])(implicit bf: BuildFrom[C, B, C2]): C2 = {
    val b = bf.newBuilder(coll)
    for (x <- iterable)
      if (p(x)) b ++= f(x)
    b.result()
  }

  /** Applies a function `f` to all elements of the outer $coll containing
    *  this `WithFilter` instance that satisfy predicate `p`.
    *
    *  @param  f   the function that is applied for its side-effect to every element.
    *              The result of function `f` is discarded.
    *
    *  @tparam  U  the type parameter describing the result of function `f`.
    *              This result will always be ignored. Typically `U` is `Unit`,
    *              but this is not necessary.
    */
  def foreach[U](f: A => U): Unit =
    for (x <- iterable)
      if (p(x)) f(x)

  /** Further refines the filter for this $coll.
    *
    *  @param q   the predicate used to test elements.
    *  @return    an object of class `WithFilter`, which supports
    *             `map`, `flatMap`, `foreach`, and `withFilter` operations.
    *             All these operations apply to those elements of this $coll which
    *             satisfy the predicate `q` in addition to the predicate `p`.
    */
  def withFilter(q: A => Boolean): WithFilter[A, C] =
    new WithFilter(coll, iterable, (a: A) => p(a) && q(a))

}
