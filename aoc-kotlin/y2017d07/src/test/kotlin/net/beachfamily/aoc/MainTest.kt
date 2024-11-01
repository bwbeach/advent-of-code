package net.beachfamily.aoc

import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class MainTest {

    @Test
    fun `examples from part1 problem statement`() {
        assertEquals(4, part1("1122"))
    }

    @Test
    fun `examples from part2 problem statement`() {
        assertEquals(4, part2("1212"))
    }


    @Test
    fun `test parseLine with sample input`() {
        assertEquals(
            Node("ktlj", 57, listOf()),
            parseLine("ktlj (57)")
        )
        assertEquals(
            Node("ugml", 68, listOf("gyxo", "ebii", "jptl")),
            parseLine("ugml (68) -> gyxo, ebii, jptl")
        )
    }
}