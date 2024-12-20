package net.beachfamily.aoc

import java.awt.Color
import javax.swing.JFrame
import javax.swing.JPanel

fun main() {
    val input = readInput("y2024d14")
    println(part1(input, 101, 103))
    println(part2(input, 101, 103))
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

fun part2(s: String, width: Int, height: Int) : Int {
    val robots = lines(s).map(::parseRobot).asSequence()
    val display = Display.Factory.create(width, height)
    var bestSoFar = 0
    for (i in generateSequence(0) { it + 1 }) {
        val positions = robots.map { it.posAfter(i, width, height) }
        val nInside = positions.filter { insideTree(it) }.count()
        if (nInside > bestSoFar) {
            bestSoFar = nInside
            println("i = $i   count = $bestSoFar")
            display.update(positions.toSet())
            Thread.sleep(1000)
        }
    }
    return 0
}

/**
 * Very simple tree scoring function.  True if the point is inside a triangle.
 */
fun insideTree(p: Point): Boolean {
    val (x, y) = p
    val x0 = if (x < 52) x else 103 - x
    return y > (52 - x0) * 18 / 10
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

class Display private constructor(
    val width: Int,
    val height: Int,
    val frame: JFrame,
    val panel: Panel,
) {
    companion object Factory {
        fun create(width: Int, height: Int): Display {
            val frame = JFrame("Day 14 - Robots")
            frame.defaultCloseOperation = JFrame.EXIT_ON_CLOSE
            frame.setSize(width * 4, height * 4)
            val panel = Panel(width, height, setOf())
            frame.add(panel)
            frame.isVisible = true
            return Display(width, height, frame, panel)
        }
    }

    fun update(robots: Set<Point>) {
        panel.update(robots)
        frame.repaint()
    }
}

class Panel(
    val w: Int,
    val h: Int,
    var robots: Set<Point>
) : JPanel(true) {
    override fun paintComponent(g: java.awt.Graphics) {
        g.color = Color.GRAY
        g.fillRect(0, 0, w * 4, h * 4)

        g.color = Color.GREEN
        for ((x, y) in robots) {
            g.fillRect(x * 4, y * 4, 4, 4)
        }
    }

    fun update(robots: Set<Point>) {
        this.robots = robots
    }
}