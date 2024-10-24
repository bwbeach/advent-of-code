package net.beachfamily.aoc

import org.junit.jupiter.api.Assertions.assertEquals
import kotlin.test.Test

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
}