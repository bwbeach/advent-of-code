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
    val bestScore = part1(s)

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

    return depthFirst(start, neighbors, bestScore, isGoal)
        .flatMap { it.second }
        .map { it.pos }
        .toSet()
        .size
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

