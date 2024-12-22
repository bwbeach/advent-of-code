package net.beachfamily.aoc

import kotlin.math.abs

fun main() {
    val input = readInput("y2024d18")
    println(part1(input, 1024, 70, 70))
    println(part2(input))
}

fun part1(s: String, n: Int, w: Int, h: Int) : Int {
    val blocked = parse(s).take(n).toSet()
    val dest = Point(w, h)
    val inRange = { p: Point -> p.x in 0 .. w && p.y in 0 .. h }
    val neighbors: (Point) -> Sequence<Pair<Point, Int>> =
        { p ->
            p.adjacentPoints().filter { it !in blocked && inRange(it) }.map { it to 1 }
        }
    val (cost, path) = aStar(
        Point(0, 0),
        { manhattan(it, dest) },
        neighbors,
        { it == dest }
    ) ?: error("No path found")

    println(makeGrid(blocked.map { it to '#' }.asSequence()))

    val items = sequence {
        yieldAll(blocked.map { it to '#' })
        yieldAll(path.map { it to 'O' })
    }
    println(makeGrid(items).toString())
    return cost
}

fun part2(s: String) : Int {
    return s.length
}

fun manhattan(a: Point, b: Point) : Int =
    abs(a.x - b.x) + abs(a.y - b.y)

fun Point.adjacentPoints(): Sequence<Point> =
    sequence {
        yield(Point(x - 1, y))
        yield(Point(x + 1, y))
        yield(Point(x, y - 1))
        yield(Point(x, y + 1))
    }

fun parse(s: String) : Sequence<Point> =
    lines(s)
        .map { parsePoint(it) }
        .asSequence()

fun parsePoint(s: String): Point =
    words(s.replace(",", " "))
        .map{ it.toInt() }
        .let { (x, y) -> Point(x, y) }

