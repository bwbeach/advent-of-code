package net.beachfamily.aoc

import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class MainTest {

    fun testInput() =
        """
        190: 10 19
        3267: 81 40 27
        83: 17 5
        156: 15 6
        7290: 6 8 6 15
        161011: 16 10 13
        192: 17 8 14
        21037: 9 7 18 13
        292: 11 6 16 20    
        """.trimIndent()

    @Test
    fun `examples from part1 problem statement`() {
        assertEquals(3749, part1(testInput()))
    }

    @Test
    fun `examples from part2 problem statement`() {
        assertEquals(4, part2("1212"))
    }
}