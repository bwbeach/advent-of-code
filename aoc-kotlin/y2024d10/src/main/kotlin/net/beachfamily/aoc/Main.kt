package net.beachfamily.aoc

fun main() {
    val input = readInput("y2024d10")
    println(part1(input))
    println(part2(input))
}

fun part1(s: String) : Int {
    val grid = gridOfInt(s)
    val posToPeaks = reachablePeaks(grid)
    return posToPeaks.values.map { it.size }.sum()
}

fun part2(s: String) : Int {
    val grid = gridOfInt(s)
    val posToPeaks = pathCount(grid)
    return posToPeaks.values.sum()
}

/**
 * Returns a Multimap from a point at a given height to all the peaks reachable from it.
 */
fun reachablePeaks(grid: Grid<Int>): Map<Point, Set<Point>> =
    flowDown(
        grid,
        0,
        { setOf(it) },
        { it.fold(setOf()) { acc, set -> acc.union(set)}}
    )

/**
 * Returns a map from a point to the number of paths to peak from that point.
 */
fun pathCount(grid: Grid<Int>): Map<Point, Int> =
    flowDown(
        grid,
        0,
        { 1 },
        { it.sum() }
    )

fun <T> flowDown(
    grid: Grid<Int>,
    fromHeight: Int,
    peakFcn: (Point) -> T,
    lowerFcn: (Sequence<T>) -> T,
): Map<Point, T> {
    val pointsAtHeight =
        grid.data
            .entries
            .asSequence()
            .filter { it.value == fromHeight }
            .map { it.key }

    val nextHigher =
        if (fromHeight == 9) {
            mapOf()
        } else {
            flowDown(grid, fromHeight + 1, peakFcn, lowerFcn)
        }

    return pointsAtHeight.associateWith {
        if (fromHeight == 9) {
            peakFcn(it)
        } else {
            lowerFcn(neighbors(it).mapNotNull { nextHigher[it] }.asSequence())
        }
    }
}

fun neighbors(p: Point): Sequence<Point> {
    return sequenceOf(
        Point(p.x - 1, p.y),
        Point(p.x + 1, p.y),
        Point(p.x, p.y - 1),
        Point(p.x, p.y + 1),
    )
}

