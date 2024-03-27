package u04lab
import u03.Sequences.* 
import Sequence.*
import u03.Optionals.* 
import Optional.*
import u04lab.Ex5Traversable.logAll

/*  Exercise 5: 
 *  - Generalise by ad-hoc polymorphism logAll, such that:
 *  -- it can be called on Sequences but also on Optional, or others... 
 *  -- it does not necessarily call log, but any function with analogous type
 *  - Hint: introduce a type class Traversable[T[_]]], capturing the ability of calling a
 *    "consumer function" on all elements (with type A) of a datastructure T[A] 
 *    Note Traversable is a 2-kinded trait (similar to Filterable, or Monad)
 *  - Write givens for Traversable[Optional] and Traversable[Sequence]
 *  - Show you can use the generalisation of logAll to:
 *  -- log all elements of an Optional, or of a Traversable
 *  -- println(_) all elements of an Optional, or of a Traversable
 */

object Ex5Traversable:

  def log[A](a: A): Unit = println("The next element is: "+a)
  
  trait Traversable[T[_]]:
    def consume[A](t: T[A])(f: A => Unit): Unit

  given Traversable[Optional] with
    def consume[A](t: Optional[A])(f: A => Unit): Unit = t match
      case Just(v) => f(v)
      case _ => ()

  given Traversable[Sequence] with
    def consume[A](t: Sequence[A])(f: A => Unit): Unit = t match
      case Cons(h, t) => f(h); consume(t)(f)
      case _ => ()

  def logAll[T, A[_]: Traversable](t: A[T]): Unit =
    val traversable = summon[Traversable[A]]
    traversable.consume(t)(log)
    
@main def test =
  val seq: Sequence[Int] = Cons(5, Cons(7, Cons(10 , Nil())))
  val opt: Optional[Int] = Just(100)
  
  logAll(seq)
  logAll(opt)