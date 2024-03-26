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
      
    extension (school: School) 
      override def courseByName(name: String): Optional[Course] = school match
        case SchoolDetail(_, courses) => find(name, courses)
      
      override def setTeacherToCourse(teacher: Teacher, course: Course): School = ???
      override def nameOfCourse(course: Course): String = course match
        case CourseDetail(name) => name
      
      override def addTeacher(name: String): School = school match
        case SchoolDetail(teachers, courses) => SchoolDetail(Cons(TeacherDetail(name, Nil()), teachers), courses)
      
      override def addCourse(name: String): School = school match
        case SchoolDetail(teachers, courses) => SchoolDetail(teachers, Cons(CourseDetail(name), courses))
      
      override def teacherByName(name: String): Optional[Teacher] = school match
        case SchoolDetail(teachers, _) => find(name, teachers)
      override def coursesOfATeacher(teacher: Teacher): Sequence[Course] = ???
      override def nameOfTeacher(teacher: Teacher): String = teacher match
        case TeacherDetail(name, _) => name
      

  
