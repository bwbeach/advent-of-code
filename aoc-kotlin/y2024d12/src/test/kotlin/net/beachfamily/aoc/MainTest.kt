package net.beachfamily.aoc

import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class MainTest {

    fun testInput1() =
        """
        AAAA
        BBCD
        BBCC
        EEEC
        """.trimIndent()

    fun testInput2() =
        """
        OOOOO
        OXOXO
        OOOOO
        OXOXO
        OOOOO
        """.trimIndent()

    fun testInput3() =
        """
        RRRRIICCFF
        RRRRIICCCF
        VVRRRCCFFF
        VVRCCCJFFF
        VVVVCJJCFE
        VVIVCCJJEE
        VVIIICJJEE
        MIIIIIJJEE
        MIIISIJEEE
        MMMISSJEEE
        """.trimIndent()

    @Test
    fun `examples from part1 problem statement`() {
        assertEquals(140, part1(testInput1()))
        assertEquals(772, part1(testInput2()))
        assertEquals(1930, part1(testInput3()))
    }

    @Test
    fun `examples from part2 problem statement`() {
        assertEquals(4, part2("1212"))
    }
}