package net.beachfamily.aoc

import kotlin.math.abs

fun seqOfSeqOfInt(input: String): Sequence<Sequence<Int>> {
    return lines(input).asSequence().map { words(it).asSequence().map { it.toInt() } }
}

fun <T : Comparable<T>> Sequence<T>.sortedSeq(): Sequence<T> {
    return this.toList().sorted().asSequence()
}

fun <T> Sequence<T>.asPair(): Pair<T, T> {
    require(this.count() == 2)
    return this.toList().let { it[0] to it[1] }
}

fun main() {
    val input = readInput("y2024d01")
    println(part1(input))
    println(part2(input))
}

fun part1(s: String) : Int {
    val input = seqOfSeqOfInt(s)
    val twoSeqs = input.transpose()
    val sorted = twoSeqs.map { it.sortedSeq() }
    val (a, b) = sorted.asPair()
    val zipped = a.zip(b)
    return zipped.map { abs(it.first - it.second) }.sum()
}

fun part2(s: String) : Int {
    return s.length
}

