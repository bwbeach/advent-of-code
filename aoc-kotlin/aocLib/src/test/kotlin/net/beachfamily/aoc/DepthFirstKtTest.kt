package net.beachfamily.aoc

import org.junit.jupiter.api.Test
import kotlin.test.assertEquals
import kotlin.test.assertNull

class DepthFirstKtTest {

    @Test
    fun case1() {
        val neighborMap = mapOf(
            0 to sequenceOf(1 to 5, 2 to 3),
            1 to sequenceOf(0 to 1, 3 to 1),
            2 to sequenceOf(0 to 1, 3 to 4),
        )

        val neighbors: (Int) -> Sequence<Pair<Int, Int>> = { neighborMap[it] ?: sequenceOf() }
        val isGoal: (Int) -> Boolean = { it == 3 }

        assertEquals(
            listOf(
                Pair(6, listOf(0, 1, 3)),
                Pair(7, listOf(0, 2, 3)),
            ),
            depthFirst(0, neighbors, 8, isGoal).toList()
        )
    }
}