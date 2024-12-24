package net.beachfamily.aoc

import java.util.PriorityQueue

/**
 * Finds all nodes in a graph reachable from a starting point.
 *
 * Each edge has a cost.  Finds the cheapest path to each reachable
 * point, returning (node, cost) pairs in cost order.
 */
fun <T> dijkstra(
    start: T,
    neighbors: (T) -> Sequence<Pair<T, Int>>,
) : Sequence<DijkstraNode<T>> =
    sequence {
        val queue = PriorityQueue<DijkstraNode<T>>()
        val done = mutableSetOf<T>()
        queue.add(DijkstraNode<T>(start, 0))
        while (!queue.isEmpty()) {
            val current = queue.remove()
            if (current.node !in done) {
                done.add(current.node)
                yield(current)
                for ((n, c) in neighbors(current.node)) {
                    queue.add(
                        DijkstraNode(
                            n,
                            current.cost + c,
                            current.node
                        )
                    )
                }
            }
        }
    }


data class DijkstraNode<T>(
    val node: T,
    val cost: Int,
    val prev: T? = null,
) : Comparable<DijkstraNode<T>> {
    override fun compareTo(other: DijkstraNode<T>): Int =
        cost.compareTo(other.cost)
}