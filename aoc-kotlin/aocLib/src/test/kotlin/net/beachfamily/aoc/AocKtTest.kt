package net.beachfamily.aoc

import com.google.common.collect.ImmutableSetMultimap
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
    fun testToSetMultimap() {
        val input = sequenceOf(
            "fruit" to "apple",
            "vegetable" to "carrot",
            "fruit" to "banana",
            "vegetable" to "lettuce"
        )
        val expected = ImmutableSetMultimap.of(
            "fruit", "apple",
            "vegetable", "carrot",
            "fruit", "banana",
            "vegetable", "lettuce",
        )
        assertEquals(expected, input.toSetMultimap())
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
        assertEquals(listOf("line1", "", "line3"), lines("\nline1\n\nline3\n\n"))
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


    @Test
    fun testTranspose() {
        // Test with a 2x3 matrix
        val matrix2x3 = sequenceOf(
            sequenceOf(1, 2, 3),
            sequenceOf(4, 5, 6, 7)
        )

        val transposed = matrix2x3.transpose().toList().map { it.toList() }

        val expectedTransposed2x3 = listOf(
            listOf(1, 4),
            listOf(2, 5),
            listOf(3, 6)
        )

        assertEquals(expectedTransposed2x3, transposed)
    }


    @Test
    fun testCountOccurrences() {
        val input = sequenceOf("apple", "banana", "apple", "orange", "banana", "apple")
        val expected = mapOf(
            "apple" to 3,
            "banana" to 2,
            "orange" to 1
        )
        assertEquals(expected, input.countOccurrences())
    }

    @Test
    fun testPairs() {
        assertEquals(listOf<Pair<Int, Int>>(), sequenceOf<Int>().pairs().toList())
        assertEquals(listOf<Pair<Int, Int>>(), sequenceOf(1).pairs().toList())
        assertEquals(listOf(1 to 2), sequenceOf(1, 2).pairs().toList())
        assertEquals(listOf(1 to 2, 2 to 3), sequenceOf(1, 2, 3).pairs().toList())
    }

    @Test
    fun testCharGrid() {
        assertEquals(
            Grid(
                0, 1,
                0, 2,
                mapOf(
                    Point(0, 0) to 'a',
                    Point(0, 1) to 'b',
                    Point(1, 1) to 'c',
                    Point(0, 2) to 'd',
                    Point(1, 2) to 'e'
                )
            ),
            gridOfChar("a\nbc\nde\n")
        )
    }
    
    
    @Test
    fun testSplitBy() {
        // Test with a sequence split by a predicate
        val input = sequenceOf(1, 2, 0, 3, 4, 0, 5)
        val expected = listOf(
            listOf(1, 2),
            listOf(3, 4),
            listOf(5)
        )
        assertEquals(expected, input.splitBy { it == 0 }.map { it.toList() }.toList())

        // Test with no splits
        val inputNoSplit = sequenceOf(1, 2, 3, 4, 5)
        val expectedNoSplit = listOf(
            listOf(1, 2, 3, 4, 5)
        )
        assertEquals(expectedNoSplit, inputNoSplit.splitBy { it == 0 }.map { it.toList() }.toList())

        // Test with an empty sequence
        val emptyInput = sequenceOf<Int>()
        val expectedEmpty = emptyList<List<Int>>()
        assertEquals(expectedEmpty, emptyInput.splitBy { it == 0 }.map { it.toList() }.toList())
    }
}

