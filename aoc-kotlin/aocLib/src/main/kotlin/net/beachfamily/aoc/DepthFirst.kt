package net.beachfamily.aoc

/**
 * Depth-first search.
 *
 * Returns a sequence of solutions that don't exceed the given cost
 */
fun <T> depthFirst(
    start: T,
    neighbors: (T) -> Sequence<Pair<T, Int>>,
    maxCost: Int,
    isGoal: (T) -> Boolean
) : Sequence<Pair<Int, List<T>>> {
    return depthFirstHelper(
        DepthFirstNode(start, 0, null),
        neighbors,
        maxCost,
        isGoal
    )
}

private fun <T> depthFirstHelper(
    pos: DepthFirstNode<T>,
    neighbors: (T) -> Sequence<Pair<T, Int>>,
    maxCost: Int,
    isGoal: (T) -> Boolean
) : Sequence<Pair<Int, List<T>>> =
    sequence {
        if (isGoal(pos.node)) {
            yield(Pair(pos.costSoFar, pos.makePath()))
        } else {
            for ((neighbor, cost) in neighbors(pos.node)) {
                val newCost = pos.costSoFar + cost
                if (newCost <= maxCost) {
                    yieldAll(
                        depthFirstHelper(
                            DepthFirstNode(neighbor, newCost, pos),
                            neighbors,
                            maxCost,
                            isGoal
                        )
                    )
                }
            }
        }
    }

private data class DepthFirstNode<T>(
    val node: T,
    val costSoFar: Int,
    val cameFrom: DepthFirstNode<T>?,
) {
    fun makePath(): List<T> {
        val result = mutableListOf<T>()
        var current: DepthFirstNode<T>? = this
        while (current != null) {
            result.add(current.node)
            current = current.cameFrom
        }
        result.reverse()
        return result.toList()
    }
}
