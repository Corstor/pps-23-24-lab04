package tasks.adts

import org.junit.*
import org.junit.Assert.*
import tasks.adts.SchoolModel.*
import u03.Sequences.*
import u03.Optionals.*
import Sequence.*
import Optional.*

class SchoolModelTest {
    val schoolModel: SchoolModule = BasicSchoolModel
    import schoolModel.*

    val mySchool: School = school(Cons(teacher("May"), Cons(teacher("Viroli"), Cons(teacher("Ricci"), Nil()))))

    @Test def teacherByName =
        assertEquals(Just(teacher("May")), mySchool.teacherByName("May"))
        assertEquals(Empty(), mySchool.teacherByName("Angelo"))
}
