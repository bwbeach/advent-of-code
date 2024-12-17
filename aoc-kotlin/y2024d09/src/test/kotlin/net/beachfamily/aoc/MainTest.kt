package net.beachfamily.aoc

import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class MainTest {

    fun testInput(): String = "2333133121414131402"

    @Test
    fun `examples from part1 problem statement`() {
        assertEquals(1928, part1(testInput()))
    }

    @Test
    fun `examples from part2 problem statement`() {
        assertEquals(2858, part2(testInput()))
    }
}