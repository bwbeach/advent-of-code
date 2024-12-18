package net.beachfamily.aoc

import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class MainTest {

    fun testInput() =
        """
        89010123
        78121874
        87430965
        96549874
        45678903
        32019012
        01329801
        10456732
        """.trimIndent()

    @Test
    fun `examples from part1 problem statement`() {
        assertEquals(36, part1(testInput()))
    }

    @Test
    fun `examples from part2 problem statement`() {
        assertEquals(81, part2(testInput()))
    }
}