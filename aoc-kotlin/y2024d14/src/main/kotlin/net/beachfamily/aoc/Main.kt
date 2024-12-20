package net.beachfamily.aoc

fun main() {
    val input = readInput("y2024d14")
    println(part1(input, 101, 103))
    println(part2(input))
}

fun part1(s: String, width: Int, height: Int) : Int {
    val robots = lines(s).map(::parseRobot)
    val positions = robots.map { it.posAfter(100, width, height) }
    val quadrants = positions.mapNotNull { it.toQuadrant(width, height) }
    val quadrantCounts = quadrants.groupingBy { it }.eachCount()
    println(quadrantCounts)
    return quadrantCounts.values.product()
}

fun Iterable<Int>.product(): Int = fold(1) { acc, i -> acc * i }

fun part2(s: String) : Int {
    return s.length
}

data class Robot(
    val pos: Point,
    val vel: Point,
) {
    fun posAfter(duration: Int, width: Int, height: Int) =
        Point(
            (pos.x + duration * vel.x).mod(width),
            (pos.y + duration * vel.y).mod(height),
        )
}

/**
 * Parses a string and returns a Robot.
 */
fun parseRobot(s: String): Robot {
    val regex = Regex("""p=(-?\d+),(-?\d+)\s+v=(-?\d+),(-?\d+)""")
    val match = regex.find(s) ?: throw IllegalArgumentException("Invalid input format")

    val (px, py, vx, vy) = match.destructured
    return Robot(
        pos = Point(px.toInt(), py.toInt()),
        vel = Point(vx.toInt(), vy.toInt())
    )
}

fun Point.toQuadrant(width: Int, height: Int): Int? {
    require( x in 0 until width && y in 0 until height)
    require(width % 2 == 1)
    require(height % 2 == 1)
    val midX = width / 2
    val midY = height / 2
    return when {
        x == midX -> null
        y == midY -> null
        x < midX && y < midY -> 1
        x < midX && y > midY -> 2
        x > midX && y < midY -> 3
        x > midX && y > midY -> 4
        else -> throw IllegalArgumentException("bug")
    }
}

