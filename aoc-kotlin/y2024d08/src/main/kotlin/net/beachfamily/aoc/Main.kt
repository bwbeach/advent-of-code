package net.beachfamily.aoc

import com.google.common.collect.ImmutableMultimap

fun main() {
    val input = readInput("y2024d08")
    println(part1(input))
    println(part2(input))
}

fun part1(s: String) : Int {
    val grid = gridOfChar(s)
    val freqToPoints = makeFreqToPoints(grid)
    return groupPointsByFrequency(freqToPoints)
        .flatMap { allNodes(it) }
        .filter { grid.data.containsKey(it) }
        .toSet()
        .size
}

private fun groupPointsByFrequency(freqToPoints: ImmutableMultimap<Char, Point>) =
    freqToPoints
        .keys()
        .toSet()
        .map { freqToPoints[it].toList() }

private fun makeFreqToPoints(grid: Grid<Char>): ImmutableMultimap<Char, Point> {
    val freqToPoints =
        grid.data.entries
            .filter { it.value != '.' }
            .map { Pair(it.value, it.key) }
            .asSequence()
            .toSetMultimap()
    return freqToPoints
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
    val grid = gridOfChar(s)
    val freqToPoints = makeFreqToPoints(grid)
    return groupPointsByFrequency(freqToPoints)
        .flatMap { allNodes2(it, grid) }
        .toSet()
        .size
}

fun allNodes2(ps: List<Point>, grid: Grid<Char>): Sequence<Point> =
    allPairs(ps).flatMap { (a, b) -> nodesForPair(a, b, grid) }

fun nodesForPair(a: Point, b: Point, grid: Grid<Char>): Sequence<Point> =
    sequence {
        yieldAll(nodeRay(a, b, grid))
        yieldAll(nodeRay(b, a, grid))
    }

fun nodeRay(a: Point, b: Point, grid: Grid<Char>): Sequence<Point> {
    val d = b - a
    return generateSequence(b) { it + d }.takeWhile { grid.data.containsKey(it) }
}
