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

    @Test def teacherByName =
        assertEquals("Viroli", schoolByTeachers.nameOfTeacher(Optional.orElse(schoolByTeachers.teacherByName("Viroli"), teacher())))

    
}
