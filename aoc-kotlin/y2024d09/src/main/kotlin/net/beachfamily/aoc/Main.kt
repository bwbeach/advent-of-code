package net.beachfamily.aoc

const val EMPTY = -1

fun main() {
    val input = readInput("y2024d09")
    println(part1(input))
    println(part2(input))
}

fun part1(s: String) : Long {
    val disk = parse(s)

    var i = 0
    var j = disk.size - 1
    while (i < j) {
        if (disk[i] != EMPTY) {
            i += 1
        } else if (disk[j] == EMPTY) {
            j -= 1
        } else {
            disk[i] = disk[j]
            disk[j] = EMPTY
            i += 1
            j -= 1
        }
    }

    return disk.withIndex()
        .filter { it.value != EMPTY }
        .map { it.value.toLong() * it.index.toLong() }
        .sum()
}

fun part2(s: String) : Int {
    return s.length
}

fun parse(input: String): MutableList<Int> {
    val result = mutableListOf<Int>()
    for ((i, c) in input.withIndex()) {
        val len = Character.getNumericValue(c)
        val label = if (i % 2 == 0)  i / 2 else EMPTY
        repeat(len) { result.add(label) }
    }
    return result
}

fun pretty(disk: List<Int>) =
    disk
        .map { if (it == EMPTY) "." else "${it}"}
        .joinToString(separator = "")

