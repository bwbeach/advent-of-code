package net.beachfamily.aoc

import org.junit.jupiter.api.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class MainTest {

    val exampleText =
        """
        pbga (66)
        xhth (57)
        ebii (61)
        havc (66)
        ktlj (57)
        fwft (72) -> ktlj, cntj, xhth
        qoyq (66)
        padx (45) -> pbga, havc, qoyq
        tknk (41) -> ugml, padx, fwft
        jptl (61)
        ugml (68) -> gyxo, ebii, jptl
        gyxo (61)
        cntj (57)
        """.trimIndent()

    val example: List<RawNode> = lines(exampleText).map { parseLine(it) }

    @Test
    fun `test part1 on example`() {
        assertEquals("tknk", part1(example))
    }

    @Test
    fun `examples from part2 problem statement`() {
        // TODO: assertEquals(60, part2(example))
    }


    @Test
    fun `test parseLine with sample input`() {
        assertEquals(
            RawNode("ktlj", 57, listOf()),
            parseLine("ktlj (57)")
        )
        assertEquals(
            RawNode("ugml", 68, listOf("gyxo", "ebii", "jptl")),
            parseLine("ugml (68) -> gyxo, ebii, jptl")
        )
    }


    @Test
    fun `test mode function with different input arrays`() {
        // Test with single mode
        assertEquals(2, mode(listOf(1, 2, 2, 3, 4).asSequence()))

        // Test with multiple modes (should return any one of the modes)
        assertTrue(listOf(2, 3).contains(mode(listOf(1, 2, 2, 3, 3, 4).asSequence())))

        // Test with no repetition
        assertTrue(listOf(1, 2, 3, 4).contains(mode(listOf(1, 2, 3, 4).asSequence())))

        // Test with all same values
        assertEquals(5, mode(listOf(5, 5, 5, 5, 5).asSequence()))
    }

}