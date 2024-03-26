package tasks.adts

import org.junit.*
import org.junit.Assert.*
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
