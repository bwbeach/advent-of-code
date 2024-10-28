package net.beachfamily.aoc

import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class MainTest {

    @Test
    fun `examples from part1 problem statement`() {
        assertEquals(0, part1(1))
        assertEquals(3, part1(12))
        assertEquals(2, part1(23))
        assertEquals(31, part1(1024))
    }

    @Test
    fun `examples from part2 problem statement`() {
        assertEquals(4, part2("1212"))
    }

    @Test
    fun `test x function`() {
        assertEquals(1, x(2, 2, 3))
        assertEquals(1, x(3, 2, 3))
        assertEquals(0, x(4, 2, 3))
        assertEquals(-1, x(5, 2, 3))
        assertEquals(-1, x(6, 2, 3))
        assertEquals(-1, x(7, 2, 3))
        assertEquals(0, x(8, 2, 3))
        assertEquals(1, x(9, 2, 3))
    }

    @Test
    fun `test y function`() {
        assertEquals(0, y(2, 2, 3))
        assertEquals(1, y(3, 2, 3))
        assertEquals(1, y(4, 2, 3))
        assertEquals(1, y(5, 2, 3))
        assertEquals(0, y(6, 2, 3))
        assertEquals(-1, y(7, 2, 3))
        assertEquals(-1, y(8, 2, 3))
        assertEquals(-1, y(9, 2, 3))
    }
}