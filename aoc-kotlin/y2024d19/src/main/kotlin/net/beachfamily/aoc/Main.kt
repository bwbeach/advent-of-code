package net.beachfamily.aoc

import java.math.BigInteger

fun main() {
    val input = readInput("y2024d19")
    println(part1(input))
    println(part2(input))
}

fun part1(s: String) : Int {
    val problem = Problem.parse(s)
    return problem.patterns
        .filter { problem.canMake(it) }
        .count()
}

fun part2(s: String) : BigInteger {
    val problem = Problem.parse(s)
    return problem.patterns
        .map { problem.waysToMake(it) }
        .sum()
}

data class Problem(
    val towels: Set<String>,
    val patterns: List<String>,
    val widestTowel: Int,
    val waysToMakeMemo: MutableMap<String, BigInteger> = mutableMapOf()
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

    fun waysToMake(pattern: String): BigInteger {
        if (pattern in waysToMakeMemo) {
            return waysToMakeMemo[pattern]!!
        }
        else if (pattern.length == 0) {
            return BigInteger.ONE
        } else {
            val result = (1 .. widestTowel)
                .map { w ->
                    if (w <= pattern.length && pattern.substring(0, w) in towels) {
                        waysToMake(pattern.substring(w))
                    } else {
                        BigInteger.ZERO
                    }
                }
                .sum()
            waysToMakeMemo[pattern] = result
            return result
        }
    }
}

private fun Iterable<BigInteger>.sum(): BigInteger =
    this.fold(BigInteger.ZERO) { acc, n -> acc + n }
