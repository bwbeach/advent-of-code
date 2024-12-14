package net.beachfamily.aoc

fun main() {
    val input = readInput("y2024d08")
    println(part1(input))
    println(part2(input))
}

fun part1(s: String) : Int {
    val grid = gridOfChar(s)
    val freqToPoints =
        grid.data.entries
            .filter { it.value != '.' }
            .map { Pair(it.value, it.key) }
            .asSequence()
            .toSetMultimap()
    return freqToPoints
        .keys()
        .toSet()
        .map { freqToPoints[it].toList() }
        .flatMap { allNodes(it) }
        .filter { grid.data.containsKey(it) }
        .toSet()
        .size
}

fun allNodes(ps: List<Point>): Sequence<Point> {
    return sequence {
        for ((a, b) in allPairs(ps)) {
            val d = b - a
            yield(b + d)
            yield(a - d)
        }
    }
}

fun part2(s: String) : Int {
    return s.length
}

