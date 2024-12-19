package net.beachfamily.aoc

import arrow.core.MemoizedDeepRecursiveFunction

fun main() {
    val input = readInput("y2024d11")
    println(part1(input))
    println(part2(input))
}

fun part1(s: String) : Long {
    val input = words(s).map { it.toLong() }.asSequence()
    return input.map { howManyMemoized(Pair(it, 25)) }.sum()
}

fun part2(s: String) : Long {
    val input = words(s).map { it.toLong() }.asSequence()
    return input.map { howManyMemoized(Pair(it, 75)) }.sum()
}

val howManyMemoized = MemoizedDeepRecursiveFunction< Pair<Long, Int>, Long> {
    (stone, depth) ->
        if (depth == 0) {
            1
        } else {
            var result = 0L
            for (s in blinkOne(stone)) {
                result += callRecursive(Pair(s, depth - 1))
            }
            result
        }
}

fun howMany(stone: Long, depth: Int): Long {
    if (depth == 0) {
        return 1
    } else {
        return blinkOne(stone).map { howMany(it, depth - 1) }.sum()
    }
}

fun blink(stones: Sequence<Long>): Sequence<Long> = stones.flatMap { blinkOne(it) }

fun blinkOne(stone: Long): Sequence<Long> =
    sequence {
        if (stone == 0L) {
            yield(1)
        } else {
            val digits = stone.toString()
            val len = digits.length
            if (len % 2 == 0) {
                yield(digits.substring(0, len / 2).toLong())
                yield(digits.substring(len / 2).toLong())
            } else {
                require(stone < Long.MAX_VALUE / 2024)
                yield(stone * 2024)
            }
        }
    }

