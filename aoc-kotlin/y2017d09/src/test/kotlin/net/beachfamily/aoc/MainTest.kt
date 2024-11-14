package net.beachfamily.aoc

import org.junit.jupiter.api.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse

class MainTest {

    @Test
    fun `examples from part2 problem statement`() {
        assertEquals(4, part2("1212"))
    }

    val garbageExamples = listOf(
        "<>" to "",
        "<random characters>" to "random characters",
        "<<<<>" to "<<<",
        "<{!>}>" to "{}",
        "<!!>" to "",
        "<!!!>>" to "",
        "<{o\"i!a,<{i<a>" to "{o\"i,<{i<a",
    )

    @Test
    fun `test parseGarbage`() {
        for ((text, expected) in garbageExamples) {
            val iterator = text.iterator()
            assertEquals('<', iterator.next())
            val actual = parseGarbage(iterator)
            assertEquals(Item.Garbage(expected), actual)
            assertFalse(iterator.hasNext())
        }
    }

    val groupCountExamples = listOf(
        "{}" to 1,
        "{{{}}}" to 3,
        "{{},{}}" to 3,
        "{{{},{},{{}}}}" to 6,
        "{<{},{},{{}}>}" to 1,
        "{<a>,<a>,<a>,<a>}" to 1,
        "{{<a>},{<a>},{<a>},{<a>}}" to 5,
        "{{<!>},{<!>},{<!>},{<a>}}" to 2,
    )

    @Test
    fun `test parseGroup`() {
        for ((text, expected) in groupCountExamples) {
            val iterator = text.iterator()
            assertEquals('{', iterator.next())
            val group = parseGroup(iterator)
            assertEquals(expected, countGroups(group))
            assertFalse(iterator.hasNext())
        }
    }

    val scoreExamples = listOf(
        "{}" to 1,
        "{{{}}}" to 6,
        "{{},{}}" to 5,
        "{<a>,<a>,<a>,<a>}" to 1,
        "{{<a!>},{<a!>},{<a!>},{<ab>}}" to 3,
    )

    @Test
    fun `testScore`() {
        for ((text, expected) in scoreExamples) {
            assertEquals(expected, part1(text))
        }
    }

    @Test
    fun `test part1b`() {
        for ((text, expected) in scoreExamples) {
            assertEquals(expected, part1b(text))
        }
    }
}