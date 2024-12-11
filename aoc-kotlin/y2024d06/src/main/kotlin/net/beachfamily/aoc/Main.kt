package net.beachfamily.aoc

import apple.laf.JRSUIConstants

fun main() {
    val input = readInput("y2024d06")
    println(part1(input))
    println(part2(input))
}

data class State(
    val pos: Point,
    val dir: Point,
    val visited: Set<Point>,
    val map: Grid<Char>
) {
    fun next(): State? {
        val newPos = pos + dir
        return when (map[newPos]) {
            null -> null
            '#' -> copy(dir = dir.turnRight())
            else -> {
                copy(pos = newPos, visited = visited + newPos)
            }
        }
    }
}

fun Point.turnRight(): Point = Point(-y, x)

fun part1(s: String) : Int {
    val grid = gridOfChar(s)
    val start = State(
        pos = findStart(grid),
        dir = Point(0, -1),
        visited = setOf(findStart(grid)),
        map = grid
    )
    return generateSequence(start) { it.next() }
        .map { it.pos }
        .toSet()
        .size
}

fun findStart(grid: Grid<Char>): Point {
    for ((k, v) in grid.data.entries) {
        if (v == '^') {
            return k
        }
    }
    throw IllegalArgumentException("No start found")
}

fun part2(s: String) : Int {
    return s.length
}

