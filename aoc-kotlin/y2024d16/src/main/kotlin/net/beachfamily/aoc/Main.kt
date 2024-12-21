package net.beachfamily.aoc

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
    val (cost, _) = aStar(start, {0}, neighbors, isGoal )!!
    return cost
}

fun part2(s: String) : Int {

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

    // Find the best cost
    val (bestCost, _) = aStar(start, {0}, neighbors, isGoal )!!
    println("bestCost = $bestCost")

    // Places to visit: the maze node to look at and the cost we can afford from
    // that point.
    val toVisit = mutableListOf(start to bestCost)

    // The nodes we've already visited.
    val visited = mutableSetOf<MazeNode>()

    // The nodes that passed the test
    val succeeded = mutableSetOf(start)

    while (toVisit.isNotEmpty()) {
        // Pull off the next unvisited node
        val (node, costAllowed) = toVisit.removeFirst()
        if (node in visited) {
            continue
        }
        visited.add(node)
        println("visited $node $costAllowed")

        // Can we reach the goal from here within the allowed cost?
        val bestFromHere = aStar(node, { 0 }, neighbors, isGoal)
        if (bestFromHere == null || bestFromHere.first > costAllowed) {
            continue
        }

        // This node works.  Add its neighbors to the search list.
        succeeded.add(node)
        for ((neighbor, stepCost) in neighbors(node)) {
            if (stepCost <= costAllowed) {
                toVisit.add(neighbor to costAllowed - stepCost)
            }
        }
    }

    return succeeded.map { it.pos }.toSet().size
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

