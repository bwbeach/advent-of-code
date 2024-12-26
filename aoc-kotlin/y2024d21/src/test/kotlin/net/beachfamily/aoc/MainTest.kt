package net.beachfamily.aoc

import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class MainTest {

    fun testInput() =
        """
        029A
        980A
        179A
        456A
        379A
        """.trimIndent()

    @Test
    fun testParseKeypad() {
        val keypad = Keypad.parse(DIR_PAD)
        assertEquals(
            mapOf(
                '<' to Point(x=0, y=1),
                '^' to Point(x=1, y=0),
                'v' to Point(x=1, y=1),
                'A' to Point(x=2, y=0),
                '>' to Point(x=2, y=1)
            ),
            keypad.charToPos
        )
        assertEquals(
            mapOf(
                Point(x = 0, y = 1) to '<',
                Point(x = 1, y = 0) to '^',
                Point(x = 1, y = 1) to 'v',
                Point(x = 2, y = 0) to 'A',
                Point(x = 2, y = 1) to '>'
            ),
            keypad.posToChar
        )
    }

    @Test
    fun testAllEncodingsForChar() {
        val keypad = Keypad.parse(DIR_PAD)
        assertEquals(
            listOf(">vA", "v>A"),
            allEncodingsForChar(
                '>',
                keypad.charToPos['^']!!,
                keypad
            ).toList()
        )
        assertEquals(
            listOf("v<A"),
            allEncodingsForChar(
                '<',
                keypad.charToPos['^']!!,
                keypad
            ).toList()
        )
    }

    @Test
    fun testCrossProduct() {
        assertEquals(
            listOf(
                listOf('a', '1'),
                listOf('a', '2'),
                listOf('a', '3'),
                listOf('b', '1'),
                listOf('b', '2'),
                listOf('b', '3'),
            ),
            crossProduct(
                sequenceOf(
                    sequenceOf('a', 'b'),
                    sequenceOf('1', '2', '3'),
                )
            ).toList().map { it.toList() }
        )
    }

    @Test
    fun testAllEncodings() {
        val numberPad = Keypad.parse(NUMBER_PAD)
        assertEquals(
            listOf("^A<<^AvA>>vA", "^A^<<AvA>>vA"),
            "341A".allEncodings(numberPad).toList()
        )
    }

    @Test
    fun firstEncodeExample() {
        val numPad = Keypad.parse(NUMBER_PAD)
        val dirPad = Keypad.parse(DIR_PAD)
        assertEquals(
            "<A^A>^^AvvvA",
            shortestEncoding("029A", numPad, 0, dirPad)
        )
        assertEquals(
            "029A",
            "<A^A>^^AvvvA".decode(numPad)
        )
    }

    @Test
    fun testSplitAfterA() {
        assertEquals(
            listOf("029A", "A"),
            "029AA".splitAfterA().toList()
        )
    }

    @Test
    fun `examples from part1 problem statement`() {
        assertEquals(126384, part1(testInput()))
    }

    @Test
    fun `examples from part2 problem statement`() {
        assertEquals(4, part2("1212"))
    }
}