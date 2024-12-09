package net.beachfamily.aoc

import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class MainTest {

//    @Test
//    fun `examples from part1 problem statement`() {
//        assertEquals(4, part1("1122"))
//    }
//
//    @Test
//    fun `examples from part2 problem statement`() {
//        assertEquals(4, part2("1212"))
//    }

    @Test
    fun testFindMuls() {
        assertEquals(
            listOf(2 to 4, 5 to 5, 11 to 8, 8 to 5),
            findMuls("""xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))""")
        )
    }
}