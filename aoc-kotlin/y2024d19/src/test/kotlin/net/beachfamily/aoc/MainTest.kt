package net.beachfamily.aoc

import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class MainTest {

    fun testInput() =
        """
        r, wr, b, g, bwu, rb, gb, br

        brwrr
        bggr
        gbbr
        rrbgbr
        ubwu
        bwurrg
        brgr
        bbrgwb
        """.trimIndent()

    @Test
    fun `examples from part1 problem statement`() {
        assertEquals(6, part1(testInput()))
    }

    @Test
    fun `examples from part2 problem statement`() {
        assertEquals(4, part2("1212"))
    }
}