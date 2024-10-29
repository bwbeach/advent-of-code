package net.beachfamily.aoc

import org.junit.jupiter.api.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class MainTest {

    @Test
    fun `test part1 valid check`() {
        assertTrue { valid("aa bb cc dd ee") }
        assertFalse { valid("aa bb cc dd aa") }
        assertTrue { valid("aa bb cc dd aaa") }
    }

    @Test
    fun `test part2 valid check`() {
        assertTrue { valid2("abcde fghij") }
        assertFalse { valid2("abcde xyz ecdab") }
        assertTrue { valid2("a ab abc abd abf abj") }
    }
}