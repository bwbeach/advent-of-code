package net.beachfamily.aoc

/**
 * A location in a grid.
 */
data class Point(val x: Int, val y: Int) {
    operator fun plus(other: Point): Point {
        return Point(x + other.x, y + other.y)
    }

    operator fun minus(other: Point): Point {
        return Point(x - other.x, y - other.y)
    }

    operator fun unaryMinus(): Point {
        return Point(-x, -y)
    }
}