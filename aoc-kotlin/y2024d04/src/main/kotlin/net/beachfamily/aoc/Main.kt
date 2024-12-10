package net.beachfamily.aoc

fun main() {
    val input = readInput("y2024d04")
    println(part1(input))
    println(part2(input))
}

fun part1(s: String) : Int {
    return countMatches("XMAS", readAllWays(gridOfChar(s)))
}

fun part2(s: String) : Int {
    val grid = gridOfChar(s)
    var count = 0
    for (y in grid.ymin + 1..grid.ymax - 1) {
        for (x in grid.xmin + 1 ..grid.xmax - 1) {
            val p = Point(x, y)
            if (grid[p] == 'A') {
                if (isMandS(grid[Point(x - 1, y - 1)], grid[Point(x + 1, y + 1)]) &&
                    isMandS(grid[Point(x - 1, y + 1)], grid[Point(x + 1, y - 1)])) {
                    count += 1
                }
            }
        }
    }
    return count
}

fun isMandS(x: Char?, y: Char?): Boolean {
    return (x == 'M' && y == 'S') || (x == 'S' && y == 'M')
}

/**
 * Scans a sequenc of chars for XMAS
 */
fun countMatches(target: String, haystack: Sequence<Char>) : Int {
    val targetList = "XMAS".asSequence().toList()
    val lastN = MutableList(target.length) { ' ' }
    var count = 0
    for (c in haystack) {
        lastN.removeFirst()
        lastN.add(c)
        if (lastN == targetList) {
            count += 1
        }
    }
    return count
}

/**
 * Reads all rows, columns, and diagonals in both directions, with a space
 * between each one.
 */
fun readAllWays(grid: Grid<Char>): Sequence<Char> =
    sequence {
        val xmin = grid.xmin
        val xmax = grid.xmax
        val ymin = grid.ymin
        val ymax = grid.ymax
        for (x in xmin .. xmax) {
            yieldAll(oneRay(grid, Point(x, ymin), Point(-1, 1), ymax - ymin + 1))
            yieldAll(oneRay(grid, Point(x, ymin), Point(0, 1), ymax - ymin + 1))
            yieldAll(oneRay(grid, Point(x, ymin), Point(1, 1), ymax - ymin + 1))

            yieldAll(oneRay(grid, Point(x, ymax), Point(-1, -1), ymax - ymin + 1))
            yieldAll(oneRay(grid, Point(x, ymax), Point(0, -1), ymax - ymin + 1))
            yieldAll(oneRay(grid, Point(x, ymax), Point(1, -1), ymax - ymin + 1))
        }
        for (y in ymin .. ymax) {
            if (y < ymax) {
                yieldAll(oneRay(grid, Point(xmin, y), Point(1, -1), xmax - xmin + 1))
            }
            yieldAll(oneRay(grid, Point(xmin, y), Point(1, 0), xmax - xmin + 1))
            if (ymin < y) {
                yieldAll(oneRay(grid, Point(xmin, y), Point(1, 1), xmax - xmin + 1))
            }

            if (y < ymax) {
                yieldAll(oneRay(grid, Point(xmax, y), Point(-1, -1), xmax - xmin + 1))
            }
            yieldAll(oneRay(grid, Point(xmax, y), Point(-1, 0), xmax - xmin + 1))
            if (ymin  < y) {
                yieldAll(oneRay(grid, Point(xmax, y), Point(-1, 1), xmax - xmin + 1))
            }
        }
    }

fun oneRay(grid: Grid<Char>, start: Point, delta: Point, len: Int) : Sequence<Char> =
    sequence {
        var p = start
        for (i in 0..<len) {
            yield(grid.data[p] ?: ' ')
            p = Point(p.x + delta.x, p.y + delta.y)
        }
        yield(' ')
    }
