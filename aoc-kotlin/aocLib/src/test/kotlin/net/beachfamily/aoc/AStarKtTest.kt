package net.beachfamily.aoc

import org.junit.jupiter.api.Test
import kotlin.test.assertEquals
import kotlin.test.assertNull

class AStarKtTest {

    @Test
    fun case1() {
        val neighborMap = mapOf(
            0 to sequenceOf(1 to 5, 2 to 3),
            1 to sequenceOf(0 to 1, 3 to 1),
            2 to sequenceOf(0 to 1, 3 to 4),
        )

        val minRemainingCost: (Int) -> Int = { 3 - it }
        val neighbors: (Int) -> Sequence<Pair<Int, Int>> = { neighborMap[it] ?: sequenceOf() }
        val isGoal: (Int) -> Boolean = { it == 3 }

        assertEquals(
            6 to listOf(0, 1, 3),
            aStar(0, minRemainingCost, neighbors, isGoal)
        )

        assertNull(
            aStar(0, minRemainingCost, neighbors, { false })
        )
    }
}