package net.beachfamily.aoc

import org.junit.jupiter.api.Test

import org.junit.jupiter.api.Assertions.*

class CircularListTest {

    @Test
    fun testEquals() {
        assertNotEquals(
            CircularList(listOf(1, 2, 3, 4, 5), 2),
            CircularList(listOf(3, 4, 5, 1, 2), 0)
        )
    }

    @Test
    fun get() {
        val list = CircularList(listOf(1, 2, 3, 4, 5), 2)
        assertEquals(2, list[-1])
        assertEquals(3, list[0])
        assertEquals(1, list[3])
    }

    @Test
    fun set() {
        val list = CircularList(listOf(1, 2, 3, 4, 5), 2)
        val newList = list.set(1, 44)
        assertEquals(3, newList[0])
        assertEquals(44, newList[1])
        assertEquals(5, newList[2])
    }

    @Test
    fun advance() {
        val before = CircularList(listOf(1, 2, 3, 4, 5), 2)
        val after = CircularList(listOf(1, 2, 3, 4, 5), 1)
        assertEquals(after, before.advance(4))
    }

    @Test
    fun reverseN() {
        val before = CircularList(listOf(1, 2, 3, 4, 5), 2)
        val after = CircularList(listOf(3, 2, 1, 5, 4), 2)
        assertEquals(after, before.reverseN(4))
    }

    @Test
    fun testToString() {
        val list = CircularList(listOf(1, 2, 3, 4, 5), 2)
        assertEquals("1 2 [3] 4 5", list.toString())
    }
}