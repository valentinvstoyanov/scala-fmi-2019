package homework1

class Queue private(private val left: List[Int] = Nil, private val right: List[Int] = Nil) extends Iterable[Int] {
  private def united: Queue = if (left.isEmpty) new Queue(right.reverse) else this

  def peek: Int = united.left.head

  def push(n: Int): Queue = new Queue(left, n :: right)

  def pop: Queue = if (isEmpty) throw new NoSuchElementException else {
    val queue = united
    new Queue(queue.left.tail, queue.right)
  }

  override def isEmpty: Boolean = left.isEmpty && right.isEmpty

  override def size: Int = left.size + right.size

  override def iterator: Iterator[Int] = (left ++ right.reverse).iterator

  override def toString: String = (left ++ right.reverse).toString
}

object Queue {
  def empty: Queue = new Queue

  def apply(xs: Seq[Int]): Queue = new Queue(xs.toList)
}
