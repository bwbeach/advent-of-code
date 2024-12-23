package net.beachfamily.aoc

fun main() {
    val input = readInput("y2024d19")
    println(part1(input))
    println(part2(input))
}

fun part1(s: String) : Int {
    val problem = Problem.parse(s)
    println(problem.towels)
    return problem.patterns
        .filter { problem.canMake(it) }
        .count()
}

fun part2(s: String) : Int {
    return s.length
}

data class Problem(
    val towels: Set<String>,
    val patterns: List<String>,
    val widestTowel: Int
) {
    companion object {
        fun parse(s: String): Problem {
            val (towels, patterns) = lines(s.replace(",", " ")).asSequence().splitBy { it.isBlank() }.asPair()
            return Problem(
                towels.flatMap { words(it) }.toSet(),
                patterns.toList(),
                towels.map { it.length }.max()
            )
        }
    }

    fun canMake(pattern: String): Boolean {
        if (pattern.length == 0) {
            return true
        } else {
            return (1 .. widestTowel).any { w ->
                w <= pattern.length &&
                        pattern.substring(0, w) in towels &&
                        canMake(pattern.substring(w))
            }
        }
    }
}

