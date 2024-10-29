package net.beachfamily.aoc

import org.junit.jupiter.api.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class MainTest {

    @Test
    fun `examples from part1 problem statement`() {
        assertEquals(4, part1("1122"))
    }

    @Test
    fun `examples from part2 problem statement`() {
        assertEquals(4, part2("1212"))
    }

    fun `test part1 valid check`() {
        assertTrue { valid("aa bb cc dd ee") }
        assertFalse { valid("aa bb cc dd aa") }
        assertTrue { valid("aa bb cc dd aaa") }
    }
}