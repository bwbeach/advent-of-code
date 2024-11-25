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
        assertEquals(12, part1(5, "3,4,1,5"))
    }

    @Test
    fun makePart2Lengths() {
        assertEquals(
            listOf(49,44,50,44,51,17,31,73,47,23),
            makePart2Lengths("1,2,3")
        )
    }

    @Test
    fun partTwo() {
        assertEquals("a2582a3a0e66e6e86e3812dcb672a272", part2(""))
        assertEquals("33efeb34ea91902bb2f59c9920caa6cd", part2("AoC 2017"))
        assertEquals("3efbe78a8d82f29979031a4aa0b16a9d", part2("1,2,3"))
        assertEquals("63960835bcdc130f0b66d7ff4f6a5a8e", part2("1,2,4"))
    }
}