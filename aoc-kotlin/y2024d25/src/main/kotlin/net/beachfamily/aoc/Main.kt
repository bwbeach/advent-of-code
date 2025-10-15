package net.beachfamily.aoc

fun main() {
    val input = readInput("y2024d25")
    println(part1(input))
    println(part2(input))
}

fun part1(s: String) : Int {
    val problem = parse(s)

    return crossProduct(problem.keys, problem.locks)
        .filter { (a, b) -> a.zip(b).all { (x, y) -> x + y <= 5 } }
        .count()
}

fun part2(s: String) : Int {
    return s.length
}

typealias Shape = List<Int>

data class Problem (
    val locks: List<Shape>,
    val keys: List<Shape>,
)

fun parse(s: String): Problem {
    val locks = mutableListOf<Shape>()
    val keys = mutableListOf<Shape>()

    for (chunk in lines(s).asSequence().splitBy { it.isBlank() }) {
        val isLock = chunk.first() == "#####"
        val shape =
            chunk
                .map { it.asSequence() }
                .transpose()
                .map { it.filter { it == '#'}.count() - 1 }
                .toList()
        if (isLock) {
            locks.add(shape)
        } else {
            keys.add(shape)
        }
    }

    return Problem(locks.toList(), keys.toList())
}

fun <T, U> crossProduct(a: List<T>, b: List<U>): Sequence<Pair<T, U>> =
    sequence {
        for (x in a) {
            for (y in b) {
                yield(Pair(x, y))
            }
        }
    }
