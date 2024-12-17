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

fun reachablePeaks(grid: Grid<Int>, fromHeight: Int): Multimap<Point, Point> {
    val pointsAtHeight =
        grid.data
            .entries
            .asSequence()
            .filter { (k, v) -> v == fromHeight }
    return if (fromHeight == 9) {
        pointsAtHeight
            .map { (k, v) -> Pair(k, k) }
            .toSetMultimap()
    } else {
        val nextHigher = reachablePeaks(grid, fromHeight + 1)
        pointsAtHeight
            .flatMap { (k, v) ->
                neighbors(k)
                    .filter { grid.data.containsKey(it) && grid.data[it] == fromHeight + 1 }
                    .flatMap { nextHigher.get(it).asSequence().map { Pair(k, it!!)} }
            }
            .toSetMultimap()
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
    return s.length
}

