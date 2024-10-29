package net.beachfamily.aoc

fun main() {
    val input = readInput("y2017d04")
    println(part1(input))
    println(part2(input))
}

fun part1(s: String) : Int {
    return lines(s).filter { valid(it) }.size
}

fun part2(s: String) : Int {
    return s.length
}

fun valid(s: String) : Boolean {
    val original = words(s)
    val withoutDups = original.toSet()
    return original.size == withoutDups.size
}

