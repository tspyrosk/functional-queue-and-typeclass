package test


/*
 Implement the following api for a FIFO (first-in, first-out) Queue with the indicated complexities
 This should be an *immutable* queue
 */
trait Queue[T] {
  // O(1)
  def isEmpty: Boolean

  // O(1)
  def insert(t: T): Queue[T]

  // O(1)
  def head: Option[T]

  // O(1) amortised
  def tail: Queue[T]
}

object Queue {
  def apply[A](l: List[A]): Queue[A] = {
    new MyQueueImpl[A](l.toStream, List.empty, l.size, 0)
  }
}

class MyQueueImpl[A](private val front:Stream[A], private val back: List[A],
    val frontSize: Long, val backSize: Long) extends Queue[A]{

  def empty[A]: Queue[A] = new MyQueueImpl[A](Stream.empty, List.empty, 0, 0)
  
  def isEmpty: Boolean = frontSize + backSize == 0
  
  def insert(t: A): Queue[A] = readjust(new MyQueueImpl[A](front, t::back, frontSize, backSize + 1))
    
    
  def head = front.headOption
  
  def tail = if(!this.isEmpty) readjust(new MyQueueImpl[A](front.tail, back, frontSize - 1, backSize))
             else this.empty
             
  def readjust[A](q: MyQueueImpl[A]): MyQueueImpl[A] = if(backSize <= frontSize) q
    else new MyQueueImpl[A](q.front ++ q.back.reverse, List.empty, frontSize + backSize, 1)
    
    
  def printQueue(name: String) = println(name + ": " + (this.front ++ this.back.reverse).toList)
}

object MyQueueTests extends App{
  val emptyQueue = Queue(List.empty)
  
  assert(emptyQueue.isEmpty)
  assert(!emptyQueue.head.isDefined)
  assert(emptyQueue.tail.isEmpty)
  
  val myQueue = Queue(List(1,2,3))
  myQueue.asInstanceOf[MyQueueImpl[Int]].printQueue("myQueue")
  
  assert(myQueue.head.get == 1)
  println("The first element in myQueue is: " + myQueue.head.get)
  
  assert(myQueue.tail.head.get == 2)
  println("The second element in myQueue is: " + myQueue.tail.head.get)
  
  val myQueue7 = myQueue.insert(4).insert(5).insert(6).insert(7)
  myQueue7.asInstanceOf[MyQueueImpl[Int]].printQueue("myQueue7")
  
  println("The 7th element in myQueue7 is: " + myQueue7.tail.tail.tail.tail.tail.tail.head.get)
  assert(myQueue7.tail.tail.tail.tail.tail.tail.head.get == 7)
  
  val myQueue567 = myQueue7.tail.tail.tail.tail
  myQueue567.asInstanceOf[MyQueueImpl[Int]].printQueue("myQueue567")
  
  println("Tests succeeded")
}