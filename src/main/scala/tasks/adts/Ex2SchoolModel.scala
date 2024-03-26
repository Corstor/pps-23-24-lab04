package tasks.adts
import u03.Sequences.*
import u03.Optionals.*
import u02.AlgebraicDataTypes.Person

/*  Exercise 2: 
 *  Implement the below trait, and write a meaningful test.
 *  Suggestion: 
 *  - reuse Sequences and Optionals as imported above
 *  - Course is a simple case classes with just the name
 *  - Teacher is a case class with name and sequence of courses
 *  - School is a case class with (sequences of) teachers and courses
 *  - add/set methods below create the new school 
 */

object SchoolModel:

  trait SchoolModule:
    type School
    type Teacher
    type Course
    def teacher(name: String): Teacher
    def course(name: String): Course
    def school(teachers: Sequence[Teacher]): School
    extension (school: School)
      def addTeacher(name: String): School
      def addCourse(name: String): School
      def teacherByName(name: String): Optional[Teacher]
      def courseByName(name: String): Optional[Course]
      def nameOfTeacher(teacher: Teacher): String
      def nameOfCourse(teacher: Teacher): String
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

    def teacher(name: String): Teacher = TeacherDetail(name, Nil())
    def course(name: String): Course = CourseDetail(name)
    def school(teachers: Sequence[Teacher]): School = SchoolDetail(teachers, Nil())

    private def findTeacher(name: String, teachers: Sequence[Teacher]): Optional[Teacher] =
        teachers match
        case Cons(h, _) if h.name == name => Just(h)
        case Cons(h, t) => findTeacher(name, t)
        case _ => Empty()

    extension (school: School) 
      override def courseByName(name: String): Optional[Course] = ???
      override def setTeacherToCourse(teacher: Teacher, course: Course): School = ???
      override def nameOfCourse(teacher: Teacher): String = ???
      override def addTeacher(name: String): School = ???
      override def addCourse(name: String): School = ???
      override def teacherByName(name: String): Optional[Teacher] = school match
        case SchoolDetail(teachers, _) => findTeacher(name, teachers)
      override def coursesOfATeacher(teacher: Teacher): Sequence[Course] = ???
      override def nameOfTeacher(teacher: Teacher): String = ???

  
