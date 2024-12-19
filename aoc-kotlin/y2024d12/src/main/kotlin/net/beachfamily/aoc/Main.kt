package net.beachfamily.aoc

fun main() {
    val input = readInput("y2024d12")
    println(part1(input))
    println(part2(input))
}

fun part1(s: String) : Int {
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
    return regions.map { it.regionScore() }.sum()
}

fun part2(s: String) : Int {
    return s.length
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



