package net.beachfamily.aoc

import org.junit.jupiter.api.Test
import kotlin.test.assertEquals
import kotlin.test.assertNull

class MainTest {

    private fun testInput(): String =
        """
        ....#.....
        .........#
        ..........
        ..#.......
        .......#..
        ..........
        .#..^.....
        ........#.
        #.........
        ......#...
        """.trimIndent()

    @Test
    fun testNext() {
        val grid = gridOfChar(testInput())
        val start = State(
            findStart(grid),
            Point(0, -1),
        )
        assertEquals(Point(4, 6), start.pos)
        assertEquals(
            start.copy(pos=Point(4, 5)),
            start.next(grid)
        )

        val s1 = start.copy(pos = Point(4, 1))
        assertEquals(
            s1.copy(dir=Point(1, 0)),
            s1.next(grid)
        )

        val s2 = start.copy(pos = Point(3, 0))
        assertNull(s2.next(grid))
    }

    @Test
    fun `examples from part1 problem statement`() {
        assertEquals(41, part1(testInput()))
    }

    @Test
    fun `examples from part2 problem statement`() {
        assertEquals(6, part2(testInput()))
    }
}