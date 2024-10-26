package net.beachfamily.aoc

fun main() {
    val input = readInput("y2017d02")
    println(part1(input))
    println(part2(input))
}

fun part1(s: String) : Int {
    val lines = lines(s)
    return lines.sumOf { part1sum(it) }
}

fun part1sum(s: String): Int {
    val numbers = words(s).map { it.toInt() }
    return numbers.max() - numbers.min()
}

fun part2(s: String) : Int {
    return lines(s)
        .map(::part2Line)
        .sum()
}


/**
 * Finds the two numbers where one evenly divides the other and returns their quotient.
 * Input is a whitespace-separated string of decimal numbers.
 */
fun part2Line(s: String) : Int {
    val numbers = words(s).map { it.toInt() }
    val pairs = allPairsBothOrders(numbers)
    val divisiblePair = singleItem( pairs.filter { (a, b) -> a % b == 0 } )
    return divisiblePair.first / divisiblePair.second
}

