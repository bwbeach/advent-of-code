package net.beachfamily.aoc

import java.util.*

fun main() {
    val input = readInput("y2024d16")
    println(part1(input))
    println(part2(input))
}

fun part1(s: String) : Int {
    val grid = gridOfChar(s)
    val start = MazeNode(grid.find('S'), Point(1, 0))
    val finish = grid.find('E')
    val neighbors = { node: MazeNode ->
        sequence {
            val p = node.pos
            val d = node.dir
            if (grid[p + d] != '#') {
                yield(MazeNode(p+d, d) to 1)
            }
            if (grid[p + d.left()] != '#') {
                yield(MazeNode(p + d.left(), d.left()) to 1001)
            }
            if (grid[p + d.right()] != '#') {
                yield(MazeNode(p + d.right(), d.right()) to 1001)
            }
        }
    }
    val isGoal = { node: MazeNode -> node.pos == finish }
    val (cost, _) = aStar(start, {0}, neighbors, isGoal )
    return cost
}

fun part2(s: String) : Int {
    return s.length
}

fun Point.left() = Point(y, -x)

fun Point.right() = Point(-y, x)

data class MazeNode(
    val pos: Point,
    val dir: Point,
)

fun <T> Grid<T>.find(needle: T): Point =
    this.data
        .entries
        .filter { it.value == needle }
        .first()
        .key

fun <T> aStar(
    start: T,
    minRemainingCost: (T) -> Int,
    neighbors: (T) -> Sequence<Pair<T, Int>>,
    isGoal: (T) -> Boolean,
) : Pair<Int, List<T>>
{
    // Nodes to look at, best ones first.
    val open = PriorityQueue<AStarNode<T>> { a, b -> a.estimatedTotalCost.compareTo(b.estimatedTotalCost) }
    open.add(AStarNode(0, start, minRemainingCost(start), null))

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
                        newCost,
                        neighbor,
                        newCost + minRemainingCost(neighbor),
                        current
                    )
                )
            }
        }
    }

    throw IllegalArgumentException("No path found")
}

data class AStarNode<T>(
    val costSoFar: Int,
    val node: T,
    val estimatedTotalCost: Int,
    val cameFrom: AStarNode<T>?
) {
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