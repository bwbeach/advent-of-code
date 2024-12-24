package net.beachfamily.aoc

import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class PointTest {

    @Test
    fun plus() {
        assertEquals(
            Point(5, 10),
            Point(1, 2) + Point(4, 8)
        )
    }

    @Test
    fun minus() {
        assertEquals(
            Point(11, 7),
            Point(15, 15) - Point(4, 8)
        )
    }

    @Test
    fun unaryMinus() {
        assertEquals(
            Point(-1, -2),
            -Point(1, 2)
        )
    }
}