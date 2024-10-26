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
    return s
        .split('\n')
        .filter { it.isNotBlank() }
        .map(::part2Line)
        .sum()
}

/**
 * Finds the two numbers where one evenly divides the other and returns their quotient.
 * Input is a whitespace-separated string of decimal numbers.
 */
fun part2Line(s: String) : Int {
    val numbers = s.split(Regex("\\s+")).filter { it.isNotBlank() }.map { it.toInt() }
    val pairs = allPairsBothOrders(numbers)
    val divisiblePair = singleItem( pairs.filter { (a, b) -> a % b == 0 } )
    return divisiblePair.first / divisiblePair.second
}