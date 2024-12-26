package net.beachfamily.aoc

import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class MainTest {

    @Test
    fun `examples from part1 problem statement`() {
        val input = "1\t9 5\n" +
                "7 5 3\n" +
                "2 4 6 8\n"
        assertEquals(18, part1(input))
    }

    @Test
    fun `examples from part2 problem statement`() {
        assertEquals(4, part2Line("5 9 2 8"))
        assertEquals(3, part2Line("9 4 7 3"))
        assertEquals(2, part2Line("3 8 6 5"))
    }
}