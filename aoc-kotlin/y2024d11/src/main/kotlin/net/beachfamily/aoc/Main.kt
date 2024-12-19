package net.beachfamily.aoc

fun main() {
    val input = readInput("y2024d11")
    println(part1(input))
    println(part2(input))
}

fun part1(s: String) : Int {
    val input = words(s).map { it.toLong() }.asSequence()
    val repeated = generateSequence(input) { blink(it)  }
    return repeated.drop(25).first().count()
}

fun part2(s: String) : Int {
    return s.length
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

