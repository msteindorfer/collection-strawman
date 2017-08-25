package strawman
package collection
package immutable

import mutable.{Builder, ImmutableBuilder}
import Hashing.computeHash

import scala.{Any, AnyRef, Array, Boolean, Int, NoSuchElementException, SerialVersionUID, Serializable, Unit, `inline`, sys}
import scala.Predef.assert
import java.lang.Integer

/** This class implements immutable sets using a hash trie.
  *
  *  '''Note:''' The builder of this hash set may return specialized representations for small sets.
  *
  *  @tparam A      the type of the elements contained in this hash set.
  *
  *  @author  Martin Odersky
  *  @author  Tiark Rompf
  *  @version 2.8
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

  import CapsuleHashSet.nullToEmpty

  def iterableFactory = CapsuleHashSet

  protected[this] def fromSpecificIterable(coll: collection.Iterable[A]): CapsuleHashSet[A] = fromIterable(coll)

  protected[this] def newSpecificBuilder(): Builder[A, CapsuleHashSet[A]] = CapsuleHashSet.newBuilder()

  def contains(elem: A): Boolean = get0(elem, computeHash(elem), 0)

  def incl(elem: A): CapsuleHashSet[A] = updated0(elem, computeHash(elem), 0)

  def excl(elem: A): CapsuleHashSet[A] = nullToEmpty(removed0(elem, computeHash(elem), 0))

  override def empty: CapsuleHashSet[A] = CapsuleHashSet.empty

  override def tail: CapsuleHashSet[A] = this - head

  override def init: CapsuleHashSet[A] = this - last

  protected def get0(key: A, hash: Int, level: Int): Boolean

  protected def updated0(key: A, hash: Int, level: Int): CapsuleHashSet[A]

  protected def removed0(key: A, hash: Int, level: Int): CapsuleHashSet[A]

}

object CapsuleHashSet extends IterableFactory[CapsuleHashSet] {

  def fromIterable[A](it: collection.Iterable[A]): CapsuleHashSet[A] =
    it match {
      case hs: CapsuleHashSet[A] => hs
      case _ => empty ++ it
    }

  def empty[A]: CapsuleHashSet[A] = EmptyCapsuleHashSet.asInstanceOf[CapsuleHashSet[A]]

  def newBuilder[A](): Builder[A, CapsuleHashSet[A]] =
    new ImmutableBuilder[A, CapsuleHashSet[A]](empty) {
      def add(elem: A): this.type = { elems = elems + elem; this }
    }

  private object EmptyCapsuleHashSet extends CapsuleHashSet[Any] {

    def iterator(): Iterator[Any] = Iterator.empty

    override def foreach[U](f: Any => U): Unit = ()

    override def head: Any = throw new NoSuchElementException("Empty Set")

    override def tail: CapsuleHashSet[Any] = throw new NoSuchElementException("Empty Set")

    override def init: CapsuleHashSet[Any] = throw new NoSuchElementException("Empty Set")

    override def size: Int = 0

    protected def get0(elem: Any, hash: Int, level: Int) = false

    protected def updated0(elem: Any, hash: Int, level: Int) = new CapsuleHashSet1(elem, hash)

    protected def removed0(key: Any, hash: Int, level: Int) = this

  }

  /**
    * Common superclass of CapsuleHashSet1 and CapsuleHashSetCollision1, which are the two possible leaves of the Trie
    */
  private[CapsuleHashSet] sealed abstract class LeafCapsuleHashSet[A] extends CapsuleHashSet[A] {
    private[CapsuleHashSet] def hash:Int
  }

  private[immutable] final class CapsuleHashSet1[A](private[CapsuleHashSet] val key: A, private[CapsuleHashSet] val hash: Int) extends LeafCapsuleHashSet[A] {

    def iterator(): Iterator[A] = Iterator.single(key)

    override def foreach[U](f: A => U): Unit = f(key)

    override def head: A = key

    override def tail: CapsuleHashSet[A] = CapsuleHashSet.empty[A]

    override def last: A = key

    override def init: CapsuleHashSet[A] = CapsuleHashSet.empty[A]

    override def size: Int = 1

    protected def get0(key: A, hash: Int, level: Int) =
      (hash == this.hash && key == this.key)

    protected def updated0(key: A, hash: Int, level: Int) =
      if (hash == this.hash && key == this.key) this
      else {
        if (hash != this.hash) {
          makeHashTrieSet(this.hash, this, hash, new CapsuleHashSet1(key, hash), level)
        } else {
          // 32-bit hash collision (rare, but not impossible)
          new CapsuleHashSetCollision1(hash, ListSet.empty + this.key + key)
        }
      }

    protected def removed0(key: A, hash: Int, level: Int) =
      if (hash == this.hash && key == this.key) null else this

  }

  private[immutable] final class CapsuleHashSetCollision1[A](private[CapsuleHashSet] val hash: Int, val ks: ListSet[A]) extends LeafCapsuleHashSet[A] {

    override def size = ks.size

    def iterator(): Iterator[A] = ks.iterator()

    override def foreach[U](f: A => U): Unit = ks.foreach(f)

    protected def get0(key: A, hash: Int, level: Int) =
      if (hash == this.hash) ks.contains(key) else false

    protected def updated0(key: A, hash: Int, level: Int): CapsuleHashSet[A] =
      if (hash == this.hash) new CapsuleHashSetCollision1(hash, ks + key)
      else makeHashTrieSet(this.hash, this, hash, new CapsuleHashSet1(key, hash), level)

    protected def removed0(key: A, hash: Int, level: Int): CapsuleHashSet[A] =
      if (hash == this.hash) {
        val ks1 = ks - key
        ks1.size match {
          case 0 =>
            // the empty set
            null
          case 1 =>
            // create a new CapsuleHashSet1 with the hash we already know
            new CapsuleHashSet1(ks1.head, hash)
          case size if size == ks.size =>
            // Should only have HSC1 if size > 1
            this
          case _ =>
            // create a new CapsuleHashSetCollision with the hash we already know and the new keys
            new CapsuleHashSetCollision1(hash, ks1)
        }
      } else this

    private def writeObject(out: java.io.ObjectOutputStream): Unit = {
      // this cannot work - reading things in might produce different
      // hash codes and remove the collision. however this is never called
      // because no references to this class are ever handed out to client code
      // and HashTrieSet serialization takes care of the situation
      sys.error("cannot serialize an immutable.CapsuleHashSet where all items have the same 32-bit hash code")
      //out.writeObject(kvs)
    }

    private def readObject(in: java.io.ObjectInputStream): Unit = {
      sys.error("cannot deserialize an immutable.CapsuleHashSet where all items have the same 32-bit hash code")
      //kvs = in.readObject().asInstanceOf[ListSet[A]]
      //hash = computeHash(kvs.)
    }

  }


  /**
    * A branch node of the HashTrieSet with at least one and up to 32 children.
    *
    * @param bitmap encodes which element corresponds to which child
    * @param elems the up to 32 children of this node.
    *              the number of children must be identical to the number of 1 bits in bitmap
    * @param size0 the total number of elements. This is stored just for performance reasons.
    * @tparam A      the type of the elements contained in this hash set.
    *
    * How levels work:
    *
    * When looking up or adding elements, the part of the hashcode that is used to address the children array depends
    * on how deep we are in the tree. This is accomplished by having a level parameter in all internal methods
    * that starts at 0 and increases by 5 (32 = 2^5) every time we go deeper into the tree.
    *
    * hashcode (binary): 00000000000000000000000000000000
    * level=0 (depth=0)                             ^^^^^
    * level=5 (depth=1)                        ^^^^^
    * level=10 (depth=2)                  ^^^^^
    * ...
    *
    * Be careful: a non-toplevel HashTrieSet is not a self-contained set, so e.g. calling contains on it will not work!
    * It relies on its depth in the Trie for which part of a hash to use to address the children, but this information
    * (the level) is not stored due to storage efficiency reasons but has to be passed explicitly!
    *
    * How bitmap and elems correspond:
    *
    * A naive implementation of a HashTrieSet would always have an array of size 32 for children and leave the unused
    * children empty (null). But that would be very wasteful regarding memory. Instead, only non-empty children are
    * stored in elems, and the bitmap is used to encode which elem corresponds to which child bucket. The lowest 1 bit
    * corresponds to the first element, the second-lowest to the second, etc.
    *
    * bitmap (binary): 00010000000000000000100000000000
    * elems: [a,b]
    * children:        ---b----------------a-----------
    */
  private[immutable] final class HashTrieSet[A](private val bitmap: Int, private[collection] val elems: Array[CapsuleHashSet[A]], private val size0: Int)
    extends CapsuleHashSet[A] {
    assert(Integer.bitCount(bitmap) == elems.length)
    // assertion has to remain disabled until SI-6197 is solved
    // assert(elems.length > 1 || (elems.length == 1 && elems(0).isInstanceOf[HashTrieSet[_]]))

    override def size = size0

    def iterator(): Iterator[A] = new TrieIterator[A](elems.asInstanceOf[Array[Iterable[A]]]) {
      final override def getElem(cc: AnyRef): A = cc.asInstanceOf[CapsuleHashSet1[A]].key
    }

    override def foreach[U](f: A => U): Unit = {
      var i = 0
      while (i < elems.length) {
        elems(i).foreach(f)
        i += 1
      }
    }

    protected def get0(key: A, hash: Int, level: Int) = {
      val index = (hash >>> level) & 0x1f
      val mask = (1 << index)
      if (bitmap == - 1) {
        elems(index & 0x1f).get0(key, hash, level + 5)
      } else if ((bitmap & mask) != 0) {
        val offset = Integer.bitCount(bitmap & (mask-1))
        elems(offset).get0(key, hash, level + 5)
      } else
        false
    }

    protected def updated0(key: A, hash: Int, level: Int) = {
      val index = (hash >>> level) & 0x1f
      val mask = (1 << index)
      val offset = Integer.bitCount(bitmap & (mask-1))
      if ((bitmap & mask) != 0) {
        val sub = elems(offset)
        val subNew = sub.updated0(key, hash, level + 5)
        if (sub eq subNew) this
        else {
          val elemsNew = java.util.Arrays.copyOf(elems, elems.length)
          elemsNew(offset) = subNew
          new HashTrieSet(bitmap, elemsNew, size + (subNew.size - sub.size))
        }
      } else {
        val elemsNew = new Array[CapsuleHashSet[A]](elems.length + 1)
        Array.copy(elems, 0, elemsNew, 0, offset)
        elemsNew(offset) = new CapsuleHashSet1(key, hash)
        Array.copy(elems, offset, elemsNew, offset + 1, elems.length - offset)
        val bitmapNew = bitmap | mask
        new HashTrieSet(bitmapNew, elemsNew, size + 1)
      }
    }

    protected def removed0(key: A, hash: Int, level: Int): CapsuleHashSet[A] = {
      val index = (hash >>> level) & 0x1f
      val mask = (1 << index)
      val offset = Integer.bitCount(bitmap & (mask-1))
      if ((bitmap & mask) != 0) {
        val sub = elems(offset)
        val subNew = sub.removed0(key, hash, level + 5)
        if (sub eq subNew) this
        else if (subNew eq null) {
          val bitmapNew = bitmap ^ mask
          if (bitmapNew != 0) {
            val elemsNew = new Array[CapsuleHashSet[A]](elems.length - 1)
            Array.copy(elems, 0, elemsNew, 0, offset)
            Array.copy(elems, offset + 1, elemsNew, offset, elems.length - offset - 1)
            val sizeNew = size - sub.size
            // if we have only one child, which is not a HashTrieSet but a self-contained set like
            // CapsuleHashSet1 or CapsuleHashSetCollision1, return the child instead
            if (elemsNew.length == 1 && !elemsNew(0).isInstanceOf[HashTrieSet[_]])
              elemsNew(0)
            else
              new HashTrieSet(bitmapNew, elemsNew, sizeNew)
          } else
            null
        } else if(elems.length == 1 && !subNew.isInstanceOf[HashTrieSet[_]]) {
          subNew
        } else {
          val elemsNew = java.util.Arrays.copyOf(elems, elems.length)
          elemsNew(offset) = subNew
          val sizeNew = size + (subNew.size - sub.size)
          new HashTrieSet(bitmap, elemsNew, sizeNew)
        }
      } else {
        this
      }
    }
  }

  // utility method to create a HashTrieSet from two leaf CapsuleHashSets (CapsuleHashSet1 or CapsuleHashSetCollision1) with non-colliding hash code)
  private def makeHashTrieSet[A](hash0:Int, elem0:CapsuleHashSet[A], hash1:Int, elem1:CapsuleHashSet[A], level:Int) : HashTrieSet[A] = {
    val index0 = (hash0 >>> level) & 0x1f
    val index1 = (hash1 >>> level) & 0x1f
    if(index0 != index1) {
      val bitmap = (1 << index0) | (1 << index1)
      val elems = new Array[CapsuleHashSet[A]](2)
      if(index0 < index1) {
        elems(0) = elem0
        elems(1) = elem1
      } else {
        elems(0) = elem1
        elems(1) = elem0
      }
      new HashTrieSet[A](bitmap, elems, elem0.size + elem1.size)
    } else {
      val elems = new Array[CapsuleHashSet[A]](1)
      val bitmap = (1 << index0)
      val child = makeHashTrieSet(hash0, elem0, hash1, elem1, level + 5)
      elems(0) = child
      new HashTrieSet[A](bitmap, elems, child.size)
    }
  }

  /**
    * In many internal operations the empty set is represented as null for performance reasons. This method converts
    * null to the empty set for use in public methods
    */
  @`inline` private def nullToEmpty[A](s: CapsuleHashSet[A]): CapsuleHashSet[A] = if (s eq null) empty[A] else s

}
