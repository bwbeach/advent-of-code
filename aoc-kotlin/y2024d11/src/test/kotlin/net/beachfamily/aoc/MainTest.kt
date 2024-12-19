package net.beachfamily.aoc

import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class MainTest {

    fun testInput() = "125 17"

    @Test
    fun `examples from part1 problem statement`() {
        assertEquals(55312, part1(testInput()))
    }

    @Test
    fun testBlinkOne() {
        assertEquals(listOf<Long>(1), blinkOne(0).toList())
        assertEquals(listOf<Long>(10, 0), blinkOne(1000).toList())
        assertEquals(listOf<Long>(2024), blinkOne(1).toList())
    }
}