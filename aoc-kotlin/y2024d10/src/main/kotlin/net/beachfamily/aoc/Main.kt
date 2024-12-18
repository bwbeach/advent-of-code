package net.beachfamily.aoc

import com.google.common.collect.Multimap

fun main() {
    val input = readInput("y2024d10")
    println(part1(input))
    println(part2(input))
}

fun part1(s: String) : Int {
    val grid = gridOfInt(s)
    val posToPeaks = reachablePeaks(grid, 0)
    return posToPeaks.entries().asSequence().count()
}

/**
 * Returns a Multimap from a point at a given height to all the peaks reachable from it.
 */
fun reachablePeaks(grid: Grid<Int>, fromHeight: Int): Multimap<Point, Point> {
    val pointsAtHeight =
        grid.data
            .entries
            .asSequence()
            .filter { it.value == fromHeight }
            .map { it.key }

    return if (fromHeight == 9) {
        pointsAtHeight
            .map { k -> Pair(k, k) }
            .toSetMultimap()
    } else {
        val nextHigher = reachablePeaks(grid, fromHeight + 1)
        pointsAtHeight
            .flatMap { k ->
                neighbors(k)
                    .filter { grid.data.containsKey(it) && grid.data[it] == fromHeight + 1 }
                    .flatMap { nextHigher.get(it).asSequence().map { Pair(k, it!!)} }
            }
            .toSetMultimap()
    }
}

/**
 * Returns a Map from a point at a given height to the number
 * of paths to a peak from that point.
 */
fun pathCount(grid: Grid<Int>, fromHeight: Int): Map<Point, Int> {
    val pointsAtHeight =
        grid.data
            .entries
            .asSequence()
            .filter { it.value == fromHeight }
            .map { it.key }

    return if (fromHeight == 9) {
        pointsAtHeight.associateWith { 1 }
    } else {
        val nextHigher = pathCount(grid, fromHeight + 1)
        pointsAtHeight
            .associateWith {
                neighbors(it).mapNotNull {nextHigher[it]}.sum()
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

fun part2(s: String) : Int {
    val grid = gridOfInt(s)
    val posToPeaks = pathCount(grid, 0)
    return posToPeaks.values.sum()
}

