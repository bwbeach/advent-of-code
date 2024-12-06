package net.beachfamily.aoc

import kotlin.math.abs

fun main() {
    val input = readInput("y2024d01")
    println(part1(input))
    println(part2(input))
}

fun part1(s: String) : Int {
    return seqOfSeqOfInt(s)
        .transpose()
        .map { it.sortedSeq() }
        .transpose()
        .map { it.asPair() }
        .map { abs(it.first - it.second) }
        .sum()
}

fun part2(s: String) : Int {
    val (list1, list2) = seqOfSeqOfInt(s).transpose().asPair()
    val counts = list2.countOccurrences()
    return list1.map { (counts[it] ?: 0) * it }.sum()
}

