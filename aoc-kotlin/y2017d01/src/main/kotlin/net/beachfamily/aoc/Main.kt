package net.beachfamily.aoc

import net.beachfamily.aoc.readInput

fun main() {
    val input = readInput("y2017d01").trim()
    println(part1(input))
    println(part2(input))
}

fun part1(s: String) : Int {
    val otherIndex = { i: Int -> (i + 1) % s.length }
    return doWork(s, otherIndex)
}

fun part2(s: String) : Int {
    val otherIndex = { i: Int -> (i + s.length / 2) % s.length }
    return doWork(s, otherIndex)
}

private fun doWork(s: String, otherIndex: (Int) -> Int): Int {
    // Extract the characters that match.
    val filtered = s.filterIndexed { index, c ->
        c == s[otherIndex(index)]
    }

    // Add them up
    return filtered.sumOf { it.digitToInt() }
}

