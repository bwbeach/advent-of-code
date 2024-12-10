package net.beachfamily.aoc

import java.util.regex.Pattern

fun main() {
    val input = readInput("y2024d03")
    println(part1(input))
    println(part2(input))
}

fun findMuls(s: String): Sequence<Pair<Int, Int>> {
    val pattern = Pattern.compile("""mul\((\d+),(\d+)\)""")
    val matcher = pattern.matcher(s)
    return sequence {
        while (matcher.find()) {
            val first = matcher.group(1).toInt()
            val second = matcher.group(2).toInt()
            yield(Pair(first, second))
        }
    }
}

fun part1(s: String) : Int {
    return findMuls(s).map { it.first * it.second }.sum()
}

fun findMuls2(s: String): Sequence<Pair<Int, Int>> {
    val pattern = Pattern.compile("""mul\((\d+),(\d+)\)|do\(\)|don't\(\)""")
    val matcher = pattern.matcher(s)
    var enabled = true
    return sequence {
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
                    yield(Pair(first, second))
                }
            } else {
                throw IllegalArgumentException("Unexpected: $whole")
            }
        }
    }
}

fun part2(s: String): Int {
    return findMuls2(s).map { it.first * it.second }.sum()
}



