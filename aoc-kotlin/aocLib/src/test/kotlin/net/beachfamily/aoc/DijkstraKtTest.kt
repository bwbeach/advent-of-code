package net.beachfamily.aoc

import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class DijkstraKtTest {

    @Test
    fun testDijkstra() {
         val neighborMap = mapOf(
             'a' to sequenceOf('b' to 5, 'c' to 3),
             'b' to sequenceOf('a' to 1, 'd' to 1),
             'c' to sequenceOf('a' to 1, 'd' to 4),
         )
         val neighbors: (Char) -> Sequence<Pair<Char, Int>> =
             { neighborMap[it] ?: sequenceOf() }
         assertEquals(
             listOf(
                 DijkstraNode('a', 0, null),
                 DijkstraNode('c', 3, 'a'),
                 DijkstraNode('b', 5, 'a'),
                 DijkstraNode('d', 6, 'b')
             ),
             dijkstra('a', neighbors).toList()
         )
    }
}