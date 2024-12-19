package net.beachfamily.aoc

fun main() {
    val input = readInput("y2024d13")
    println(part1(input))
    println(part2(input))
}

fun part1(s: String) : Int {
    return parseProblems(s)
        .mapNotNull { it.solve() }
        .map { (a, b) -> a * 3 + b }
        .sum()
}

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
    return Problem(ax.toInt(), ay.toInt(), bx.toInt(), by.toInt(), px.toInt(), py.toInt())
}

fun part2(s: String) : Int {
    return s.length
}

data class Problem(
    val ax: Int,
    val ay: Int,
    val bx: Int,
    val by: Int,
    val px: Int,
    val py: Int,
) {
    fun solve(): Pair<Int, Int>? {
        println("$this")
        val det = (ax * by) - (ay * bx)
        require(det != 0)
        val detA = (px * by) - (py * bx)
        val detB = (ax * py) - (ay * px)
        println("$det $detA $detB ${detA % det} ${detB % det}")
        if (detA % det != 0 || detB % det != 0) {
            return null
        } else {
            return Pair(detA / det, detB / det)
        }
    }
}



