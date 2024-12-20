package net.beachfamily.aoc

import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class MainTest {

    fun testInput() =
        """
        p=0,4 v=3,-3
        p=6,3 v=-1,-3
        p=10,3 v=-1,2
        p=2,0 v=2,-1
        p=0,0 v=1,3
        p=3,0 v=-2,-2
        p=7,6 v=-1,-3
        p=3,0 v=-1,-2
        p=9,3 v=2,3
        p=7,3 v=-1,2
        p=2,4 v=2,-3
        p=9,5 v=-3,-3
        """.trimIndent()

    @Test
    fun testPosAfter() {
        val robot = parseRobot("p=2,4 v=2,-3")
        assertEquals(Point(2, 4), robot.posAfter(0, 11, 7))
        assertEquals(Point(4, 1), robot.posAfter(1, 11, 7))
        assertEquals(Point(6, 5), robot.posAfter(2, 11, 7))
        assertEquals(Point(8, 2), robot.posAfter(3, 11, 7))
        assertEquals(Point(10, 6), robot.posAfter(4, 11, 7))
        assertEquals(Point(1, 3), robot.posAfter(5, 11, 7))
    }

    @Test
    fun `examples from part1 problem statement`() {
        assertEquals(12, part1(testInput(), 11, 7))
    }

    @Test
    fun `examples from part2 problem statement`() {
        assertEquals(4, part2("1212"))
    }
}