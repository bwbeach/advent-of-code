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
    

    @Test
    fun testLines() {
        // Test with a multi-line string
        assertEquals(listOf("line1", "line2", "line3"), lines("line1\nline2\nline3"))

        // Test with a single-line string, with a trailing newline
        assertEquals(listOf("single_line"), lines("single_line\n"))

        // Test with an empty string
        assertEquals(listOf<String>(), lines(""))

        // Test with a string containing empty lines
        assertEquals(listOf("line1", "line3"), lines("line1\n\nline3"))
    }

    @Test
    fun testWords() {
        // Test with a simple sentence
        assertEquals(listOf("This", "is", "a", "test"), words("This is a test"))

        // Test with multiple spaces
        assertEquals(listOf("This", "is", "a", "test"), words("This   is  a    test"))

        // Test with punctuation
        assertEquals(
            listOf("Hello,", "world!", "This", "is", "a", "test."),
            words("Hello, world! This is a test.")
        )

        // Test with an empty string
        assertEquals(listOf<String>(), words(""))

        // Test with a string of whitespace
        assertEquals(listOf<String>(), words(" \n\t "))
    }
}

