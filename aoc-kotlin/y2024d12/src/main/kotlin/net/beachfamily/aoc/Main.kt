package net.beachfamily.aoc

fun main() {
    val input = readInput("y2024d12")
    println(part1(input))
    println(part2(input))
}

fun part1(s: String) : Int {
    val regions = findRegions(s)
    return regions.map { it.regionScore() }.sum()
}

fun part2(s: String) : Int {
    val regions = findRegions(s)
    return regions.map { it.regionScore2() }.sum()
}

private fun findRegions(s: String): Sequence<Set<Point>> {
    val grid = gridOfChar(s)
    val toAssign = grid.data.toMutableMap()
    val regions = sequence {
        while (toAssign.isNotEmpty()) {
            val (seedPos, seedType) = toAssign.entries.first()
            toAssign.remove(seedPos)
            val toVisit = seedPos.neighbors().toMutableList()
            val region = mutableSetOf(seedPos)
            while (toVisit.isNotEmpty()) {
                val p = toVisit.removeFirst()
                if (toAssign[p] == seedType) {
                    region.add(p)
                    toAssign.remove(p)
                    toVisit.addAll(p.neighbors())
                }
            }
            yield(region.toSet())
        }
    }
    return regions
}

fun Point.neighbors(): Sequence<Point> =
    sequence {
        yield( Point(this@neighbors.x + 1, this@neighbors.y))
        yield( Point(this@neighbors.x - 1, this@neighbors.y))
        yield( Point(this@neighbors.x, this@neighbors.y + 1))
        yield( Point(this@neighbors.x, this@neighbors.y - 1))
    }

fun Set<Point>.regionScore() : Int {
    val perimiter =
        this
            .flatMap { it.neighbors() }
            .filter { it !in this }
            .count()
    val area = this.size
    return perimiter * area
}

fun Set<Point>.regionScore2() : Int {
    val edges = numberOfEdges(this)
    val area = this.size
    return edges * area
}

fun numberOfEdges(region: Set<Point>) : Int =
    verticalEdges(region) + verticalEdges(region.map(Point::flip).toSet())

fun Point.flip() = Point(y, x)

fun Point.mirror() = Point(-x, y)

fun verticalEdges(region: Set<Point>) : Int =
    leftEdges(region) + leftEdges(region.map(Point::mirror).toSet())
/**
 * Returns a list of positions on the grid whose upper-left corner is
 * the top end of a vertical section of fence on the left side of the region.
 */
fun leftEdges(region: Set<Point>) : Int {
    val segments = sequence {
        for (p in region) {
            val left = Point(p.x - 1, p.y)
            if (left !in region) {
                yield(p)
            }
        }
    }.toList().sorted()
    return 1 + segments
        .zipWithNext()
        .filter { it.first + Point(0, 1) != it.second }
        .count()
}

