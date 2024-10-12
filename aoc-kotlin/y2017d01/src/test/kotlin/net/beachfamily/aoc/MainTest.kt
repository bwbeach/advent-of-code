package net.beachfamily.aoc

import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class MainTest {

    @Test
    fun `examples from problem statement`() {
        assertEquals(3, part1("1122"))
        assertEquals(4, part1("1111"))
        assertEquals(0, part1("1234"))
        assertEquals(9, part1("91212129"))
    }
}