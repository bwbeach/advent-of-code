package net.beachfamily.aoc

import org.junit.jupiter.api.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class MainTest {

    @Test
    fun `examples from part1 problem statement`() {
        val p = Program(mutableListOf(0, 3, 0, 1, -3), 0)
        p.step()
        assertEquals(Program(mutableListOf(1, 3, 0, 1, -3), 0, 1), p)
        p.step()
        assertEquals(Program(mutableListOf(2, 3, 0, 1, -3), 1, 2), p)
        p.step()
        assertEquals(Program(mutableListOf(2, 4, 0, 1, -3), 4, 3), p)
        p.step()
        assertEquals(Program(mutableListOf(2, 4, 0, 1, -2), 1, 4), p)
        assertFalse(p.isDone())
        p.step()
        assertEquals(Program(mutableListOf(2, 5, 0, 1, -2), 5, 5), p)
        assertTrue(p.isDone())
    }

    @Test
    fun `examples from part2 problem statement`() {
        val p = Program(mutableListOf(0, 3, 0, 1, -3), 0)
        while (!p.isDone()) {
            p.step2()
        }
        assertEquals(Program(mutableListOf(2, 3, 2, 3, -1), 5, 10), p)
    }
}