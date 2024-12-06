package net.beachfamily.aoc

fun main() {
    val input = readInput("y2024d02")
    println(part1(input))
    println(part2(input))
}

fun part1(s: String): Int {
    return seqOfSeqOfInt(s).filter { isOkPart1(it) }.count()
}

fun isOkPart1(seq: Sequence<Int>) : Boolean {
    val deltas = seq.zipWithNext { a, b -> b - a }
    return deltas.all { 1 <= it && it <= 3 } || deltas.all { -3 <= it && it <= -1 }
}

fun part2(s: String) : Int {
    return s.length
}

