package net.beachfamily.aoc

import org.junit.jupiter.api.Test
import kotlin.test.assertEquals
import kotlin.test.assertNull

class MainTest {

    @Test
    fun `examples from part1 problem statement`() {
        assertEquals(5, part1(listOf(0, 2, 7, 0)))
    }

    @Test
    fun `examples from part2 problem statement`() {
        assertEquals(4, part2(listOf(0, 2, 7, 0)))
    }

    @Test
    fun `test indexOfBiggest`() {
        assertEquals(2, indexOfBiggest(sequenceOf(0, 2, 7, 0)))
        assertEquals(0, indexOfBiggest(sequenceOf(3, 1, 2, 3)))
    }

    @Test
    fun `test indexOfFirstRepeated`() {
        assertNull(indexOfFirstRepeated(sequenceOf(0, 2, 7)))
        assertEquals(3, indexOfFirstRepeated(sequenceOf(3, 1, 2, 3)))
    }
    
    @Test 
    fun `test firstCycle`() {
        assertEquals(2 to 5, firstCycle(sequenceOf(0, 1, 2, 3, 4, 2, 3, 4)))
    }

    @Test
    fun `test step by step`() {
        
        val start = listOf(0, 2, 7, 0)
        val firstSix = generateSequence(start) { step(it) }.take(6).toList()

        assertEquals(
            listOf(
                listOf(0, 2, 7, 0),
                listOf(2, 4, 1, 2),
                listOf(3, 1, 2, 3),
                listOf(0, 2, 3, 4),
                listOf(1, 3, 4, 1),
                listOf(2, 4, 1, 2)
            ),
            firstSix
        )
    }
}