package net.beachfamily.aoc

fun main() {
    val input = readInput("y2024d22")
    println(part1(input))
    println(part2(input))
}

fun part1(s: String) : Long =
    lines(s)
        .map { it.toLong() }
        .map { generateSequence(it) { n -> n.nextSecret() } }
        .sumOf { it.drop(2000).first() }

fun part2(s: String) : Int {
    return s.length
}

fun Long.nextSecret(): Long {
    val a = this.mix(this.shl(6))
    val b = a.mix(a.shr(5))
    return b.mix(b.shl(11))
}

fun Long.mix(b: Long): Long =
    (this.xor(b)).and(0xFFFFFF)

