package net.beachfamily.aoc

import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class MainTest {

    @Test
    fun `examples from part1 problem statement`() {
        assertEquals(3, part1("1122"))
        assertEquals(4, part1("1111"))
        assertEquals(0, part1("1234"))
        assertEquals(9, part1("91212129"))
    }

    @Test
    fun `examples from part2 problem statement`() {
        assertEquals(6, part2("1212"))
        assertEquals(0, part2("1221"))
        assertEquals(4, part2("123425"))
        assertEquals(12, part2("123123"))
        assertEquals(4, part2("12131415"))
    }
}