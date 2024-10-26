package net.beachfamily.aoc

import org.junit.jupiter.api.Assertions.assertEquals
import kotlin.test.Test
import kotlin.test.assertFailsWith

class AocKtTest {

    @Test
    fun testAllPairs() {
        val input = listOf(1, 2, 3)
        val expected = listOf(
            Pair(1, 2),
            Pair(1, 3),
            Pair(2, 3)
        )

        assertEquals(expected, allPairs(input).toList())
    }


    @Test
    fun testAllPairsBothOrders() {
        val input = listOf(1, 2, 3)
        val expected = listOf(
            Pair(1, 2),
            Pair(2, 1),
            Pair(1, 3),
            Pair(3, 1),
            Pair(2, 3),
            Pair(3, 2)
        )

        assertEquals(expected, allPairsBothOrders(input).toList())
    }

    @Test
    fun testSingleItem() {
        // Test with an empty list

        // What the assistant wrote
        val empty = sequenceOf<Int>()
        assertFailsWith<IllegalArgumentException> {
            singleItem(empty)
        }

        // Test with one item
        val single = sequenceOf(42)
        val expectedSingleItem = 42
        assertEquals(expectedSingleItem, singleItem(single))

        // Test with more than one item
        val multipleItemsList = sequenceOf(1, 2, 3)
        assertFailsWith<IllegalArgumentException> {
            singleItem(multipleItemsList)
        }
    }
}

