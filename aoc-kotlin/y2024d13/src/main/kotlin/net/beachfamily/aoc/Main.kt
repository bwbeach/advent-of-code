package net.beachfamily.aoc

import java.math.BigInteger

fun main() {
    val input = readInput("y2024d13")
    println(part1(input))
    println(part2(input))
}

fun part1(s: String) : BigInteger {
    return parseProblems(s)
        .mapNotNull { it.solve() }
        .map { (a, b) -> a * 3.toBigInteger() + b }
        .sum()
}

fun part2(s: String) : BigInteger {
    return parseProblems(s)
        .map { it.toPart2Problem() }
        .mapNotNull { it.solve() }
        .map { (a, b) -> a * 3.toBigInteger() + b }
        .sum()
}

fun Sequence<BigInteger>.sum(): BigInteger =
    this.fold(BigInteger.ZERO) { acc, i -> acc + i }

fun parseProblems(s : String): Sequence<Problem> {
    return lines(s)
        .asSequence()
        .splitBy { it.isBlank() }
        .map { parseProblemInput(it.joinToString("\n")) }
}

fun parseProblemInput(input: String): Problem {
    val regex = """Button A: X\+(\d+), Y\+(\d+)\nButton B: X\+(\d+), Y\+(\d+)\nPrize: X=(\d+), Y=(\d+)""".toRegex()
    val matchResult = regex.find(input) ?: throw IllegalArgumentException("Input format does not match the expected pattern.")

    val (ax, ay, bx, by, px, py) = matchResult.destructured
    return Problem(
        ax.toBigInteger(),
        ay.toBigInteger(),
        bx.toBigInteger(),
        by.toBigInteger(),
        px.toBigInteger(),
        py.toBigInteger()
    )
}

data class Problem(
    val ax: BigInteger,
    val ay: BigInteger,
    val bx: BigInteger,
    val by: BigInteger,
    val px: BigInteger,
    val py: BigInteger,
) {
    fun solve(): Pair<BigInteger, BigInteger>? {
        val det = (ax * by) - (ay * bx)
        require(det != BigInteger.ZERO)
        val detA = (px * by) - (py * bx)
        val detB = (ax * py) - (ay * px)
        if (detA % det != BigInteger.ZERO || detB % det != BigInteger.ZERO) {
            return null
        } else {
            return Pair(detA / det, detB / det)
        }
    }

    fun toPart2Problem(): Problem {
        return Problem(
            ax,
            ay,
            bx,
            by,
            px + 10000000000000.toBigInteger(),
            py + 10000000000000.toBigInteger()
        )
    }
}
