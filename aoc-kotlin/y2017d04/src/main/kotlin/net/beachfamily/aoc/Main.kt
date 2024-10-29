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
    return lines(s).filter { valid2(it) }.size
}

fun valid(s: String) : Boolean {
    val original = words(s)
    val withoutDups = original.toSet()
    return original.size == withoutDups.size
}

fun valid2(s: String) : Boolean {
    val original = words(s).map { sortedString(it) }
    val withoutDups = original.toSet()
    return original.size == withoutDups.size
}

fun sortedString(s: String) : String {
    return s.toList().sorted().joinToString("")
}

