package net.beachfamily.aoc

import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class MainTest {

    fun testInput() =
        """
        Register A: 729
        Register B: 0
        Register C: 0

        Program: 0,1,5,4,3,0
        """.trimIndent()

    @Test
    fun example1() {
        val computer = Computer.create(0, 0, 9, listOf(2, 6))
        computer.run()
        assertEquals(computer.b, 1.toBigInteger())
    }

    @Test
    fun example2() {
        val computer = Computer.create(10, 0, 0, listOf(5,0,5,1,5,4))
        assertEquals(
            listOf(0, 1, 2).map { it.toBigInteger() },
            computer.run()
        )
    }

    @Test
    fun example3() {
        val computer = Computer.create(2024, 0, 0, listOf(0,1,5,4,3,0))
        assertEquals(
            listOf(4,2,5,6,7,7,7,7,3,1,0).map { it.toBigInteger() },
            computer.run()
        )
        assertEquals(0.toBigInteger(), computer.a)
    }

    @Test
    fun example4() {
        val computer = Computer.create(0, 29, 0, listOf(1,7))
        computer.run()
        assertEquals(26.toBigInteger(), computer.b)
    }

    @Test
    fun example5() {
        val computer = Computer.create(0, 2024, 43690, listOf(4,0))
        computer.run()
        assertEquals(44354.toBigInteger(), computer.b)
    }

    @Test
    fun `examples from part1 problem statement`() {
        assertEquals("4,6,3,5,6,3,5,2,1,0", part1(testInput()))
    }

    @Test
    fun `examples from part2 problem statement`() {
        assertEquals(4, part2("1212"))
    }
}