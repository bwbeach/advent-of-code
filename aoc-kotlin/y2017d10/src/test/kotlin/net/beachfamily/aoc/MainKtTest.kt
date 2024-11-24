package net.beachfamily.aoc

import org.junit.jupiter.api.Test

import org.junit.jupiter.api.Assertions.*

class MainKtTest {

    @Test
    fun oneStep() {
        val s0 = State(CircularList((0..4).toList(), 0), 0)
        assertEquals("State(items=[0] 1 2 3 4, skip=0)", s0.toString())
        val s1 = oneStep(s0, 3)
        assertEquals("State(items=2 1 0 [3] 4, skip=1)", s1.toString())
        val s2 = oneStep(s1, 4)
        assertEquals("State(items=4 3 0 [1] 2, skip=2)", s2.toString())
        val s3 = oneStep(s2, 1)
        assertEquals("State(items=4 [3] 0 1 2, skip=3)", s3.toString())
        val s4 = oneStep(s3, 5)
        assertEquals("State(items=3 4 2 1 [0], skip=4)", s4.toString())
    }

    @Test
    fun partOne() {
        assertEquals(12, part1(5, listOf(3, 4, 1, 5)))

    }
}