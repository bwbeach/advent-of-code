package net.beachfamily.aoc

data class Equation(
    val testValue: Long,
    val numbers: Sequence<Long>,
)

fun main() {
    val input = readInput("y2024d07")
    println(part1(input))
    println(part2(input))
}

fun part1(s: String) : Int {
    return s.length
}

fun part2(s: String) : Int {
    return s.length
}

fun parse(s: String): Sequence<Equation> =
    lines(s).map { parseEquation(it) }.asSequence()

fun parseEquation(s: String): Equation {
    val (a, b) = s.split(":")
    return Equation(
        a.toLong(),
        words(b).map { it.toLong() }.asSequence()
    )
}