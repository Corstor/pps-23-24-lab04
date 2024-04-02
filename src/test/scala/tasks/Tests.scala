package tasks

import org.junit.*
import org.junit.Assert.*
import tasks.adts.Ex1ComplexNumbers.*

//Task 1
class ComplexTest:

  // Choice of implementation to test
  val complexADT: ComplexADT = BasicComplexADT
  import complexADT.*

  // From now, everything is independent of specific implementation of Complex

  @Test def testReal() =
    assertEquals(10, complex(10, 20).re(), 0)

  @Test def testImaginary() =
    assertEquals(20, complex(10, 20).im(), 0)

  @Test def testSum() =
    assertEquals(complex(11, 22), complex(10, 20) sum complex(1, 2))

  @Test def testSubtract() =
    assertEquals(complex(9, 18), complex(10, 20) subtract complex(1, 2))

  @Test def testAsString() =
    assertEquals("10.0 + 5.0i", complex(10.0, 5.0).asString())

  @Test def optionalTestAdvancedAsString() =
    assertEquals("0.0", complex(0, 0).asString())
    assertEquals("10.0", complex(10.0, 0).asString())
    assertEquals("10.0 + 5.0i", complex(10.0, 5.0).asString())
    assertEquals("10.0 - 5.0i", complex(10.0, -5.0).asString())
    assertEquals("5.0i", complex(0, 5.0).asString())
    assertEquals("-5.0i", complex(0, -5.0).asString())

//Task 2

import tasks.adts.SchoolModel.*
import u03.Sequences.*
import u03.Optionals.*
import Sequence.*
import Optional.*
import u02.AlgebraicDataTypes.Person
import tasks.adts.SchoolModel.BasicSchoolModel.TeacherDetail

class SchoolModelTest {
    val schoolModel: SchoolModule = BasicSchoolModel
    import schoolModel.*

    val schoolByTeachers: School = school().addTeacher("Viroli")
    val schoolByCourses: School = school().addCourse("PPS")
    val schoolByAll: School = school().setTeacherToCourse(getTeacherByName(schoolByTeachers, "Viroli"), getCourseByName(schoolByCourses, "PPS"))

    def getTeacherByName(school: School, name: String): Teacher =
        Optional.orElse(school.teacherByName(name), teacher())

    def getCourseByName(school: School, name: String): Course =   
        Optional.orElse(school.courseByName(name), course())

    @Test def teacherByName =
        assertEquals("Viroli", schoolByTeachers.nameOfTeacher(getTeacherByName(schoolByTeachers, "Viroli")))

    @Test def courseByName =
        assertEquals("PPS", schoolByCourses.nameOfCourse(getCourseByName(schoolByCourses, "PPS")))
    
    @Test def coursesOfATeacher =
        assertEquals(Cons(getCourseByName(schoolByAll, "PPS"), Nil()), schoolByAll.coursesOfATeacher(getTeacherByName(schoolByAll ,"Viroli")))
}

import tasks.adts.Ex3Stacks.StackImpl
import u03.Sequences.Sequence
import u03.Optionals.Optional

//Task 3
class Stacktest:

  val stack = StackImpl

  import stack.*

  @Test def testEmpty() =
    assertEquals(Sequence.Nil(), empty[Int].asSequence())

  @Test def testPush() =
    assertEquals(Sequence.Cons(10, Sequence.Nil()), empty[Int].push(10).asSequence())

  @Test def testPopOnEmpty() =
    assertEquals(Optional.Empty(), empty[Int].pop(10))

  @Test def testPopOnNotEmpty() =
    assertEquals(Optional.Just((10, Sequence.Nil())), empty[Int].push(10).pop(10))


import tasks.Ex4Summables.*
import Ex4Summables.*

//Task 4
@main def trySummables =
  val si = Cons(10, Cons(20, Cons(30, Nil())))  
  println:
    sumAllInt(si) // 60 

  println:
    sumAll(si) // 60

  val sd = Cons(10.0, Cons(20.0, Cons(30.0, Nil())))  
  println:
    sumAll(sd) // 60.0

  val ss = Cons("10", Cons("20", Cons("30", Nil())))  
  println:
    sumAll(ss) // "102030"

//Task 5

import tasks.Ex5Traversable.*
import Ex5Traversable.*

@main def test =
  val seq: Sequence[Int] = Cons(5, Cons(7, Cons(10 , Nil())))
  val opt: Optional[Int] = Just(100)
  
  logAll(seq)
  logAll(opt)

// Task 6

@main def main: Unit = 
  import Ex6TryModel.*

  val result = for 
    a <- success(10)
    b <- success(30)
  yield a + b

  assert(result.getOrElse(-1) == 40)

  val result2 = for 
    a <- success(10)
    b <- failure(new RuntimeException("error"))
    c <- success(30)
  yield a + c

  assert(success(20).map(_ + 10).getOrElse(-1) == 30)
  assert(result2.getOrElse(-1) == -1)

  val result3 = for
    a <- exec(10)
    b <- exec(throw new RuntimeException("error"))
    c <- exec(30)
  yield a + c