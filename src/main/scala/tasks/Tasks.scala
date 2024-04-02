package tasks


//Task 1, svolto da solo

object Ex1ComplexNumbers:

  trait ComplexADT:
    type Complex
    def complex(re: Double, im: Double): Complex
    extension (complex: Complex)
      def re(): Double
      def im(): Double
      def sum(other: Complex): Complex
      def subtract(other: Complex): Complex
      def asString(): String

  object BasicComplexADT extends ComplexADT:

    case class ComplexNumber(re: Double, im: Double)

    type Complex = ComplexNumber
    def complex(re: Double, im: Double): Complex = ComplexNumber(re, im)
    extension (complex: Complex)
      def re(): Double = complex match
        case ComplexNumber(re, _) => re
    
      def im(): Double = complex match
        case ComplexNumber(_, im) => im
      def sum(other: Complex): Complex = ComplexNumber(complex.re + other.re, complex.im + other.im)
      
      def subtract(other: Complex): Complex = ComplexNumber(complex.re - other.re, complex.im - other.im)
      def asString(): String = complex match
        case ComplexNumber(re, 0) => re.toString()
        case ComplexNumber(0, im) => im + "i"
        case ComplexNumber(re, im) if im > 0 => re + " + " + im + "i"
        case _ => complex.re + " - " + complex.im.abs + "i"


import u03.Sequences.*
import u03.Optionals.*
import u02.AlgebraicDataTypes.Person

//Task 2, svolto da solo

object SchoolModel:

  trait SchoolModule:
    type School
    type Teacher
    type Course
    def course(): Course
    def teacher(): Teacher
    def school(): School
    extension (school: School)
      def addTeacher(name: String): School
      def addCourse(name: String): School
      def teacherByName(name: String): Optional[Teacher]
      def courseByName(name: String): Optional[Course]
      def nameOfTeacher(teacher: Teacher): String
      def nameOfCourse(course: Course): String
      def setTeacherToCourse(teacher: Teacher, course: Course): School
      def coursesOfATeacher(teacher: Teacher): Sequence[Course]

  object BasicSchoolModel extends SchoolModule:
    import Sequence.*
    import Optional.*

    case class CourseDetail(name: String)
    case class TeacherDetail(name: String, courses: Sequence[Course])
    case class SchoolDetail(teachers: Sequence[Teacher], courses: Sequence[Course])

    type Course = CourseDetail
    type Teacher = TeacherDetail
    type School = SchoolDetail

    def course(): Course = CourseDetail("")
    def teacher(): Teacher = TeacherDetail("", Nil())
    def school(): School = SchoolDetail(Nil(), Nil())

    private def find[A](name: String, seq: Sequence[A]): Optional[A] =
        seq match
        case Cons(h, _) if h match
          case CourseDetail(n) => n == name
          case TeacherDetail(n, _) => n == name
         => Just(h)
        case Cons(h, t) => find(name, t)
        case _ => Empty()
    
    private def updateTeachers(course: Course, teacher: TeacherDetail, teachers: Sequence[Teacher]): Sequence[TeacherDetail] =
      teachers match
        case Nil() => Cons(TeacherDetail(teacher.name, Cons(course, teacher.courses)) , Nil())
        case _ => Sequence.map(teachers)(t => t match
          case TeacherDetail(name, courses) if name == teacher.name => TeacherDetail(name, Cons(course, courses))
          case _ => t
      )
      
    extension (school: School) 
      override def courseByName(name: String): Optional[Course] = school match
        case SchoolDetail(_, courses) => find(name, courses)
      
      override def setTeacherToCourse(teacher: Teacher, course: Course): School = school match
        case SchoolDetail(teachers, courses) => SchoolDetail(updateTeachers(course, teacher, teachers), school.courseByName(course.name) match
          case Empty() => Cons(course, courses)
          case _ => courses
      )
      
      override def nameOfCourse(course: Course): String = course match
        case CourseDetail(name) => name
      
      override def addTeacher(name: String): School = school match
        case SchoolDetail(teachers, courses) => SchoolDetail(Cons(TeacherDetail(name, Nil()), teachers), courses)
      
      override def addCourse(name: String): School = school match
        case SchoolDetail(teachers, courses) => SchoolDetail(teachers, Cons(CourseDetail(name), courses))

      override def teacherByName(name: String): Optional[Teacher] = school match
        case SchoolDetail(teachers, _) => find(name, teachers)

      override def coursesOfATeacher(teacher: Teacher): Sequence[Course] =
        Optional.orElse(school.teacherByName(teacher.name), TeacherDetail("", Nil())).courses

      override def nameOfTeacher(teacher: Teacher): String = teacher match
        case TeacherDetail(name, _) => name


//Task 3, svolto da solo

object Ex3Stacks:

  trait StackADT:
    type Stack[A]
    def empty[A]: Stack[A] // factory
    extension [A](stack: Stack[A])
      def push(a: A): Stack[A]
      def pop(a: A): Optional[(A, Stack[A])]
      def asSequence(): Sequence[A]
  
  object StackImpl extends StackADT:
    import Sequence.*
    import Optional.*

    type Stack[A] = Sequence[A]
    def empty[A]: Stack[A] = Nil()

    extension [A](stack: Stack[A])
      def push(a: A): Stack[A] = stack match
        case _ => Cons(a, stack)
      
      def pop(a: A): Optional[(A, Stack[A])] = stack match
        case Cons(h, t) => Just(h, t)
        case _ => Empty()
      
      def asSequence(): Sequence[A] = stack

import Sequence.*

//task 4, svolto da solo

object Ex4Summables:

  def sumAllInt(seq: Sequence[Int]): Int = seq match
    case Cons(h, t) => h + sumAllInt(t)
    case _ => 0

  trait Summable[A]:
    def sum(a1: A, a2: A): A
    def zero: A

  def sumAll[A: Summable](seq: Sequence[A]): A = 
    val summable = summon[Summable[A]]
    @annotation.tailrec
    def _sumAll(seq: Sequence[A])(temp: A): A = 
      seq match
        case Cons(h, t) => _sumAll(t)(summable.sum(h, temp))
        case _ => temp
    _sumAll(seq)(summable.zero)
      
    

  given Summable[Int] with
    def sum(a1: Int, a2: Int): Int = a1 + a2
    def zero: Int = 0
  
  // write givens for Summable[Double] and Summable[String]

  given Summable[Double] with
    def sum(a1: Double, a2: Double): Double = a1 + a2
    def zero: Double = 0

  given Summable[String] with
    def sum(a1: String, a2: String): String = a1 + a2
    def zero: String = ""

import Optional.*

//Task 5, svolto da solo

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

import u04.monads.Monads.Monad
import u02.CaseMatch.res

//Task 6, svolto da solo

object Ex6TryModel:
  private enum TryImpl[A]:
    case Success(value: A)
    case Failure(exception: Throwable)

  opaque type Try[A] = TryImpl[A]

  def success[A](value: A): Try[A] = TryImpl.Success(value)
  def failure[A](exception: Throwable): Try[A] = TryImpl.Failure(exception)
  def exec[A](expression: => A): Try[A] = try success(expression) catch failure(_)

  extension [A](m: Try[A])
    def getOrElse[B >: A](other: B): B = m match
      case TryImpl.Success(value) => value
      case TryImpl.Failure(_) => other

  given Monad[Try] with
    override def unit[A](value: A): Try[A] = success(value)
    extension [A](m: Try[A]) 

      override def flatMap[B](f: A => Try[B]): Try[B] = m match
        case TryImpl.Success(value) => f(value)
        case TryImpl.Failure(exception) => failure(exception)