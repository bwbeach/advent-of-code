package net.beachfamily.aoc

import kotlin.math.abs

fun main() {
    val input = readInput("y2024d20")
    println(part1(input, 100))
    println(part2(input))
}

fun part1(s: String, minValue: Int) : Int {
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
    println("BEST = $bestCost")
    // The names of all the cheats
    val cheats: () -> Sequence<Cheat> = {
        sequence {
            for (p1 in costFromStart.keys) {
                for (p2 in p1.fourNeighbors()) {
                    if (maze[p2] == '#') {
                        for (p3 in p2.fourNeighbors()) {
                            if (p3 != p1 && costToEnd.containsKey(p3)) {
                                yield(Cheat(p1, p3))
                            }
                        }
                    }
                }
            }
        }
    }
    val cheatValue: (Cheat)-> Int? = { cheat ->
        val fromStart = costFromStart[cheat.p1]!!
        val toEnd = costToEnd[cheat.p2]!!
        val benefit = bestCost - (fromStart + 2 + toEnd)
        if (benefit <= 0) {
            null
        } else {
            benefit
        }
    }
    return cheats().mapNotNull { cheatValue(it) }.filter { it >= minValue }.count()
}

data class Cheat(
    val p1: Point,
    val p2: Point,
)

fun manhattan(a: Point, b: Point): Int {
    return abs(a.x - b.x) + abs(a.y - b.y)
}

fun part2(s: String) : Int {
    return s.length
}

