package net.beachfamily.aoc

fun main() {
    val input = readInput("y2024d06")
    println(part1(input))
    println(part2(input))
}

data class State(
    val pos: Point,
    val dir: Point,
) {
    fun next(map: Grid<Char>): State? {
        val newPos = pos + dir
        return when (map[newPos]) {
            null -> null
            '#' -> copy(dir = dir.turnRight())
            else -> {
                copy(pos = newPos)
            }
        }
    }
}

fun Point.turnRight(): Point = Point(-y, x)

fun findStart(grid: Grid<Char>): Point {
    for ((k, v) in grid.data.entries) {
        if (v == '^') {
            return k
        }
    }
    throw IllegalArgumentException("No start found")
}

fun Grid<Char>.hasRepeat(): Boolean {
    val start = State(
        pos = findStart(this),
        dir = Point(0, -1),
    )
    return generateSequence(start) { it.next(this) }.hasRepeat()
}

fun part1(s: String) : Int {
    val grid = gridOfChar(s)
    val start = State(
        pos = findStart(grid),
        dir = Point(0, -1),
    )
    return generateSequence(start) { it.next(grid) }
        .map { it.pos }
        .toSet()
        .size
}

fun part2(s: String) : Int {
    val grid = gridOfChar(s)
    return grid
        .allPoints()
        .filter { grid[it] == '.' }
        .filter { grid.update(it, '#').hasRepeat() }
        .count()
}

