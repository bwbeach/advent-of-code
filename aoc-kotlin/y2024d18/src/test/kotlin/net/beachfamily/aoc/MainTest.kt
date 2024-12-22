package net.beachfamily.aoc

import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class MainTest {

    fun testInput() =
        """
        5,4
        4,2
        4,5
        3,0
        2,1
        6,3
        2,4
        1,5
        0,6
        3,3
        2,6
        5,1
        1,2
        5,5
        2,5
        6,5
        1,4
        0,4
        6,4
        1,1
        6,1
        1,0
        0,5
        1,6
        2,0
        """.trimIndent()

    @Test
    fun `examples from part1 problem statement`() {
        assertEquals(22, part1(testInput(), 12, 6, 6))
    }

    @Test
    fun `examples from part2 problem statement`() {
        assertEquals("6,1", part2(testInput(), 6, 6))
    }
}