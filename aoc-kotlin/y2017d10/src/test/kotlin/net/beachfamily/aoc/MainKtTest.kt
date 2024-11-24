package net.beachfamily.aoc

import org.junit.jupiter.api.Test

import org.junit.jupiter.api.Assertions.*

class MainKtTest {

    @Test
    fun oneStep() {
        val tmp = (0.. 4).toList()
        val s0 = State(CircularList(tmp, 0), 0)
        assertEquals("State(items=[0] 1 2 3 4, skip=0)", s0.toString())
        val s1 = oneStep(s0, 3)
        assertEquals("State(items=[3] 4 2 1 0, skip=1)", s1.toString())
        val s2 = oneStep(s1, 4)
        assertEquals("State(items=[1] 2 4 3 0, skip=2)", s2.toString())
        val s3 = oneStep(s2, 1)
        assertEquals("State(items=[3] 0 1 2 4, skip=3)", s3.toString())
        val s4 = oneStep(s3, 5)
        assertEquals("State(items=[0] 3 4 2 1, skip=4)", s4.toString())
    }
}