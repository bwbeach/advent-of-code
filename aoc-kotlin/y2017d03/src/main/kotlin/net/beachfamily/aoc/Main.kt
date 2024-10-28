package net.beachfamily.aoc

fun main() {
    val input = readInput("y2017d03")
    val address = singleItem(words(input).asSequence()).toInt()
    val letters = listOf('a', 'b', 'c')
    
    println(part1(address))
    println(part2(input))
}

fun part1(a: Int) : Int {
    return a
}

fun part2(s: String) : Int {
    return s.length
}

