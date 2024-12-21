package net.beachfamily.aoc

import java.util.PriorityQueue

/**
 * A* search algorithm.
 *
 * https://en.wikipedia.org/wiki/A*_search_algorithm
 */
fun <T> aStar(
    start: T,
    minRemainingCost: (T) -> Int,
    neighbors: (T) -> Sequence<Pair<T, Int>>,
    isGoal: (T) -> Boolean,
) : Pair<Int, List<T>>?
{
    // Nodes to look at, best ones first.
    val open = PriorityQueue<AStarNode<T>> { a, b -> a.estimatedTotalCost.compareTo(b.estimatedTotalCost) }
    open.add(AStarNode(start, 0, minRemainingCost(start), null))

    // Best score yet for a node
    val bestYet = mutableMapOf<T, Int>()

    while (open.isNotEmpty()) {
        val current = open.remove()
        if (current.node !in bestYet || current.costSoFar < bestYet[current.node]!!) {
            bestYet[current.node] = current.costSoFar
            if (isGoal(current.node)) {
                return Pair(current.costSoFar, current.makePath())
            }
            for ((neighbor, cost) in neighbors(current.node)) {
                val newCost = current.costSoFar + cost
                open.add(
                    AStarNode(
                        neighbor,
                        newCost,
                        newCost + minRemainingCost(neighbor),
                        current
                    )
                )
            }
        }
    }

    return null
}

/**
 * A node in A* search decorated with cost and history.
 */
private data class AStarNode<T>(
    // Where we are
    val node: T,
    // The cost to get here from the start point.
    val costSoFar: Int,
    // Estimated minimum cost to goal, including the cost to get here.
    val estimatedTotalCost: Int,
    // The previous step in the search.
    val cameFrom: AStarNode<T>?
) {
    /**
     * Returns the path from the start to this node, built by
     * following the `cameFrom` back links.
     */
    fun makePath(): List<T> {
        val result = mutableListOf<T>()
        var current: AStarNode<T>? = this
        while (current != null) {
            result.add(current.node)
            current = current.cameFrom
        }
        result.reverse()
        return result.toList()
    }
}