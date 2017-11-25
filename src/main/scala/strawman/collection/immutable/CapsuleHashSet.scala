package strawman
package collection
package immutable

import java.lang.Thread
import java.util.{Comparator, NoSuchElementException}
import java.util.concurrent.atomic.AtomicReference

import io.usethesource.capsule.core.PersistentTrieSet
import io.usethesource.capsule.core.PersistentTrieSet.SetResult

import strawman.collection.Hashing.computeHash
import strawman.collection.mutable.{Builder, ImmutableBuilder, ReusableBuilder}
import scala.{Any, AnyRef, Array, Boolean, Int, NoSuchElementException, SerialVersionUID, Serializable, Unit, `inline`, sys}
import scala.Predef.assert
import scala.runtime.BoxesRunTime

/** This class implements immutable sets using a hash trie.
  *
  *  '''Note:''' The builder of this hash set may return specialized representations for small sets.
  *
  *  @tparam A      the type of the elements contained in this hash set.
  *
  *  @author  Michael J. Steindorfer
  *  @version 2.x
  *  @since   2.3
  *  @define Coll `immutable.CapsuleHashSet`
  *  @define coll immutable hash set
  */
@SerialVersionUID(2L)
sealed trait CapsuleHashSet[A]
  extends Set[A]
    with SetOps[A, CapsuleHashSet, CapsuleHashSet[A]]
    with StrictOptimizedIterableOps[A, CapsuleHashSet, CapsuleHashSet[A]]
    with Serializable {

  def iterableFactory = CapsuleHashSet

  protected[this] def fromSpecificIterable(coll: collection.Iterable[A]): CapsuleHashSet[A] = fromIterable(coll)

  protected[this] def newSpecificBuilder(): Builder[A, CapsuleHashSet[A]] = CapsuleHashSet.newBuilder()

  def contains(elem: A): Boolean

  def incl(elem: A): CapsuleHashSet[A]

  def excl(elem: A): CapsuleHashSet[A]

  override def empty: CapsuleHashSet[A] = CapsuleHashSet.empty

  override def tail: CapsuleHashSet[A] = this - head

  override def init: CapsuleHashSet[A] = this - last

}

object CapsuleHashSet extends IterableFactory[CapsuleHashSet] {

  private val EMPTY_SET = new HashTrieSet(PersistentTrieSet.EMPTY_NODE, 0, 0)

  def fromIterable[A](it: collection.Iterable[A]): CapsuleHashSet[A] =
    it match {
      case hs: CapsuleHashSet[A] => hs
      case _ => {
        //  empty ++ it
        val builder = newBuilder[A]()
        it.foreach(item => builder += item)
        builder.result
      }
    }

  def empty[A]: CapsuleHashSet[A] = EMPTY_SET.asInstanceOf[CapsuleHashSet[A]]

//  def newBuilder[A](): Builder[A, CapsuleHashSet[A]] =
//    new ImmutableBuilder[A, CapsuleHashSet[A]](empty) {
//      def add(elem: A): this.type = { elems = elems + elem; this }
//    }

  def newBuilder[A](): Builder[A, CapsuleHashSet[A]] = new HashTrieSetBuilder[A](
    PersistentTrieSet.EMPTY_NODE.asInstanceOf[PersistentTrieSet.AbstractSetNode[A]], 0, 0)

  private[immutable] final class HashTrieSetBuilder[A](private var rootNode: PersistentTrieSet.AbstractSetNode[A],
                                                       private var cachedHashCode: Int,
                                                       private var cachedSize: Int)
    extends ReusableBuilder[A, CapsuleHashSet[A]] {

    private val mutator = new AtomicReference[Thread](Thread.currentThread)

    override def clear(): Unit = {
      rootNode = PersistentTrieSet.EMPTY_NODE.asInstanceOf[PersistentTrieSet.AbstractSetNode[A]]
      cachedHashCode = 0
      cachedSize = 0
    }

    override def add(elem: A) = {
      if (mutator.get == null) throw new java.lang.IllegalStateException("Transient already frozen.")

      val elemHash = elem.hashCode // computeHash(elem)
      val details = SetResult.unchanged[A]

      val newRootNode = rootNode.updated(mutator, elem, elemHash, 0, details)

      if (details.isModified) {
        rootNode = newRootNode
        cachedHashCode += elemHash
        cachedSize += 1
      }

      this
    }

    override def result() = {
      mutator set null
      new HashTrieSet[A](rootNode, cachedHashCode, cachedSize)
    }
  }

  private[immutable] final class HashTrieSet[A](private[immutable] val rootNode: PersistentTrieSet.AbstractSetNode[A],
                                                private[immutable] val cachedHashCode: Int,
                                                private[immutable] val cachedSize: Int)
    extends CapsuleHashSet[A] {

    override def size = cachedSize

    override def contains(elem: A): Boolean = {
      val elemHash = elem.hashCode // computeHash(elem)
      rootNode.contains(elem, elemHash, 0)
    }

    override def incl(elem: A): CapsuleHashSet[A] = {
      val elemHash = elem.hashCode // computeHash(elem)
      val details = PersistentTrieSet.SetResult.unchanged[A]

      val newRootNode = rootNode.updated(null, elem, elemHash, 0, details)

      if (details.isModified)
        new HashTrieSet[A](newRootNode, cachedHashCode + elemHash, cachedSize + 1)
      else this
    }

    override def excl(elem: A): CapsuleHashSet[A] = {
      val elemHash = elem.hashCode // computeHash(elem)
      val details = PersistentTrieSet.SetResult.unchanged[A]

      val newRootNode = rootNode.removed(null, elem, elemHash, 0, details)

      if (details.isModified)
        new HashTrieSet[A](newRootNode, cachedHashCode - elemHash, cachedSize - 1)
      else this
    }

    override def iterator = new SetKeyIterator[A](rootNode)

    protected class SetKeyIterator[A](val rootNode: PersistentTrieSet.AbstractSetNode[A])
      extends PersistentTrieSet.AbstractSetIterator[A](rootNode) with Iterator[A] {

      override def next: A = {
        if (!hasNext) throw new NoSuchElementException
        return currentValueNode.getKey({currentValueCursor += 1; currentValueCursor - 1})
      }
    }

    override def equals(that: Any): Boolean =
      that match {
        case set: HashTrieSet[A] =>
          (this eq set) ||
            (cachedSize == set.cachedSize) &&
              (cachedHashCode == set.cachedHashCode) &&
              (rootNode.equivalent(set.rootNode,
                (x: scala.AnyRef, y: scala.AnyRef) => (x eq y) || BoxesRunTime.equals2(x, y)))
        case _ => super.equals(that)
      }

    override def hashCode(): Int = collection.Set.setHash(toIterable)
  }

}
