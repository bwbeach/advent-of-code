package net.beachfamily.aoc

import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class MainTest {

    val example =
        """
        b inc 5 if a > 1
        a inc 1 if b < 5
        c dec -10 if a >= 1
        c inc -20 if c == 10
        """.trimIndent()

    val program = lines(example).map { Instruction.parse(it) }

    @Test
    fun `examples from part1 problem statement`() {
        assertEquals(1, part1(program))
    }

    @Test
    fun `examples from part2 problem statement`() {
        assertEquals(10, part2(program))
    }
}