package net.beachfamily.aoc

import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class MainTest {

    @Test
    fun `examples from part1 problem statement`() {
        val testInput =
            """
            MMMSXXMASM
            MSAMXMSMSA
            AMXSXMAAMM
            MSAMASMSMX
            XMASAMXAMM
            XXAMMXXAMA
            SMSMSASXSS
            SAXAMASAAA
            MAMMMXMMMM
            MXMXAXMASX
            """.trimIndent()
        assertEquals(18, part1(testInput))
    }

    @Test
    fun `examples from part2 problem statement`() {
        assertEquals(4, part2("1212"))
    }

    @Test
    fun testReadAllWays() {
        val grid = gridOfChar("ab\ncd\n")
        val actual = readAllWays(grid).joinToString("")
        assertEquals(
            "a  ac ad c  ca cb bc bd b  da db d  a  ab b  ba cd c  dc d  ",
            actual
        )
    }
}