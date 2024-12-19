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
    fun `examples from part2 problem statement`() {
        assertEquals(4, part2("1212"))
    }

    @Test
    fun testBlinkOne() {
        assertEquals(listOf(1), blinkOne(0).toList())
        assertEquals(listOf(10, 0), blinkOne(1000).toList())
        assertEquals(listOf(2024), blinkOne(1).toList())
    }
}