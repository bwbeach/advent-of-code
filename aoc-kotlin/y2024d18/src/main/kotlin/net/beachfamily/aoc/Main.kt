package net.beachfamily.aoc

import kotlin.math.abs

fun main() {
    val input = readInput("y2024d18")
    println(part1(input, 1024, 70, 70))
    println(part2(input, 70, 70,))
}

fun part1(s: String, n: Int, w: Int, h: Int) : Int {
    val blocked = parse(s).take(n).toSet()
    return findCost(blocked, w, h)!!
}

fun part2(s: String, w: Int, h: Int) : String {
    val blocked = mutableSetOf<Point>()
    for (p in parse(s)) {
        blocked.add(p)
        if (findCost(blocked, w, h) == null) {
            return "${p.x},${p.y}"
        }
    }
    throw IllegalArgumentException("No point blocks all paths")
}

fun findCost(blocked: Set<Point>, w: Int, h: Int): Int? {
    val dest = Point(w, h)
    val result = aStar(
        Point(0, 0),
        { manhattan(it, dest) },
        neighborsFcn(blocked, w, h),
        { it == dest }
    )
    return result?.first
}

fun neighborsFcn(blocked: Set<Point>, w: Int, h: Int): (Point) -> Sequence<Pair<Point, Int>> {
    val inRange = { p: Point -> p.x in 0 .. w && p.y in 0 .. h }
    return { p ->
        p.adjacentPoints().filter { it !in blocked && inRange(it) }.map { it to 1 }
    }
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

