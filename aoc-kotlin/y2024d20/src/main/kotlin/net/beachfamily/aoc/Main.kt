package net.beachfamily.aoc

import kotlin.math.abs

fun main() {
    val input = readInput("y2024d20")
    println(solve(input, 2, 100))
    println(solve(input, 20, 100))
}

fun solve(s: String, maxLength: Int, minValue: Int) : Int {
    val maze = gridOfChar(s)
    val start = maze.find('S')
    val end = maze.find('E')
    val neighbors: (Point) -> Sequence<Pair<Point, Int>> = { p ->
        p.fourNeighbors()
            .filter { maze.data.containsKey(it) && maze[it] != '#' }
            .map { it to 1 }
    }
    val costFromStart = dijkstra(start, neighbors).map { it.node to it.cost }.toMap()
    val costToEnd = dijkstra(end, neighbors).map { it.node to it.cost }.toMap()
    val (bestCost, _) = aStar(start, { manhattan(it, end) }, neighbors, { it == end })!!
    val cheatValue: (Cheat)-> Int? = { cheat ->
        val fromStart = costFromStart[cheat.p1]!!
        val toEnd = costToEnd[cheat.p2]!!
        val benefit = bestCost - (fromStart + manhattan(cheat.p1, cheat.p2) + toEnd)
        if (benefit <= 0) {
            null
        } else {
            benefit
        }
    }
    return Cheat.allCheats(maze, maxLength)
        .mapNotNull { cheatValue(it) }
        .filter { it >= minValue }
        .count()
}

data class Cheat(
    val p1: Point,
    val p2: Point,
) {
    companion object {
        /**
         * Returns a sequence of all cheats up to maxLength.
         */
        fun allCheats(maze: Grid<Char>, maxLength: Int): Sequence<Cheat> {
            // For the purpose of making cheats, all locations in the grid are
            // valid places to travel, including '#'.
            val neighbors: (Point) -> Sequence<Pair<Point, Int>> = { p ->
                p.fourNeighbors()
                    .filter { maze.data.containsKey(it) }
                    .map { it to 1 }
            }

            // Cheats must start and end on valid places to be in the maze, not walls.
            val startOrEndChars = setOf('.', 'S', 'E')
            val canBeStartOrEnd: (Point) -> Boolean = { maze[it] in startOrEndChars }

            // The set of points that can start a cheat.
            val startPoints = maze.data.keys.asSequence().filter(canBeStartOrEnd)

            // The set of end points for a given start point.
            val endPoints: (Point) -> Sequence<Point> = { p0 ->
                dijkstra(p0, neighbors)
                    .takeWhile { it.cost <= maxLength }
                    .map { it.node }
                    .filter(canBeStartOrEnd)
            }

            // Build the cheats
            return startPoints.flatMap { p0 -> endPoints(p0).map { pn -> Cheat(p0, pn) }}
        }
    }
}

fun manhattan(a: Point, b: Point): Int {
    return abs(a.x - b.x) + abs(a.y - b.y)
}

