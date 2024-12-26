package net.beachfamily.aoc

import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class MainTest {

    fun testInput() =
        """
        1
        10
        100
        2024
        """.trimIndent()

    @Test
    fun testNextSecret() {
        val expectedSequence: Sequence<Long> = sequenceOf(
            123,
            15887950,
            16495136,
            527345,
            704524,
            1553684,
            12683156,
            11100544,
            12249484,
            7753432,
            5908254,
        )
        for ((a, b) in expectedSequence.zipWithNext()) {
            assertEquals(b, a.nextSecret())
        }
    }

    @Test
    fun `examples from part1 problem statement`() {
        assertEquals(37327623, part1(testInput()))
    }

    @Test
    fun `examples from part2 problem statement`() {
        assertEquals(4, part2("1212"))
    }


}