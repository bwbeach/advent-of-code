package net.beachfamily.aoc

private const val input = "the input"

fun main() {
    val input = readInput("y2017d02")
    println(part1(input))
    println(part2(input))
}

fun part1(s: String) : Int {
    val lines = s.split('\n').filter { it.isNotBlank() }
    return lines.sumOf { part1sum(it) }
}

fun part1sum(s: String): Int {
    val words = s.split('\t', ' ').filter { it.isNotBlank() }
    val wordInts = words.map { it.toInt() }
    return wordInts.max() - wordInts.min()
}

fun part2(s: String) : Int {
    return s.length
}

