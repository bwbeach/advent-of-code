package net.beachfamily.aoc

import java.util.regex.Pattern

fun main() {
    val input = readInput("y2024d03")
    println(part1(input))
    println(part2(input))
}

fun findMuls(s: String): List<Pair<Int, Int>> {
    val pattern = Pattern.compile("""mul\((\d+),(\d+)\)""")
    val matcher = pattern.matcher(s)
    val result = mutableListOf<Pair<Int, Int>>()
    while (matcher.find()) {
        val first = matcher.group(1).toInt()
        val second = matcher.group(2).toInt()
        result.add(Pair(first, second))
    }
    return result
}

fun part1(s: String) : Int {
    return findMuls(s).map { it.first * it.second }.sum()
}

fun findMuls2(s: String): List<Pair<Int, Int>> {
    val pattern = Pattern.compile("""mul\((\d+),(\d+)\)|do\(\)|don't\(\)""")
    val matcher = pattern.matcher(s)
    val result = mutableListOf<Pair<Int, Int>>()
    var enabled = true
    while (matcher.find()) {
        val whole = matcher.group()
        if (whole == "do()") {
            enabled = true
        } else if (whole == "don't()") {
            enabled = false
        } else if (whole.startsWith("mul(")) {
            val first = matcher.group(1).toInt()
            val second = matcher.group(2).toInt()
            if (enabled) {
                result.add(Pair(first, second))
            }
        } else {
            throw IllegalArgumentException("Unexpected: $whole")
        }
    }
    return result
}

fun part2(s: String): Int {
    return findMuls2(s).map { it.first * it.second }.sum()
}



